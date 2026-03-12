(ns thetis.core
  (:require [thetis.state :as state]
            [thetis.compiler.core :as compiler]
            [thetis.compiler.forms :as forms]
            [thetis.functions.parse :as parse]
            [thetis.utils.misc :as u]
            [clojure.string :as str]))

(defonce prototypes (atom {}))

;; Runtime guard dispatch table.
;; Shape: {generic-fullname → arity → [{:pred (fn [x] ...) :impl (fn [args] ...)} ...]}
;; Guards are checked in order — first match wins, then falls through to protocol dispatch.
(defonce guard-impls (atom {}))

;; === Build hooks ===

(defn reset-state!
  "Full state reset — used as shadow-cljs build hook.
   Nukes all compile-time state and rebuilds from scratch.
   For incremental builds, use `prepare-state!` instead."
  {:shadow.build/stage :compile-prepare}
  [build-stage]
  (tap> "reset thetis state")
  (reset! prototypes {})
  (reset! guard-impls {})
  (state/reset!)
  (state/init-types!)
  build-stage)

(defn prepare-state!
  "Incremental build hook — preserves state across builds.
   Only resets the namespace preparation tracker so that
   namespaces being recompiled will clean their own contributions.
   Use this instead of `reset-state!` to enable incremental builds."
  {:shadow.build/stage :compile-prepare}
  [build-stage]
  (tap> "prepare thetis state (incremental)")
  (state/reset-prepared!)
  build-stage)

;; === Internal helpers ===

(defn- current-compiler
  "Build a thetis compiler value from the current state.
   Must be called within a macro expansion context."
  []
  (assoc (state/get)
         :expansion {:env (state/env) :form (state/form)}))

(defn- prepare-ns!
  "Prepare a namespace for (re)compilation.
   On first call for a given ns in a build pass: removes old contributions.
   Subsequent calls are no-ops (idempotent for incremental builds)."
  [ns-str]
  (when-not (state/ns-prepared? ns-str)
    (state/swap! compiler/remove-ns-contributions ns-str)
    (state/mark-ns-prepared! ns-str)))

(defn- get-spec
  "Look up a function spec by (potentially aliased) name."
  [name]
  (clojure.core/get (state/get :functions) (state/qualify-symbol name)))

(defn- get-spec!
  "Look up a function spec by name, throws if not found."
  [name]
  (or (get-spec name)
      (throw (ex-info (str "Cannot find spec " (pr-str (state/qualify-symbol name)))
                      {:available (keys (state/get :functions))}))))

;; === Public macros ===

(u/defmac defguard
  "Define a predicate guard type.
   A guard is a refinement of a base type with an additional predicate.
   Guards dispatch at runtime via a cond-chain before protocol dispatch.

   (defguard :positive :base :number :pred pos?)"
  [guard-key & {:keys [base pred]}]
  (let [current-ns (str (u/ns-sym))]
    (prepare-ns! current-ns)
    (state/swap! compiler/register-guard current-ns guard-key {:base base :pred pred})
    nil))

(u/defmac defg
  "Create a generic function."
  [& form]
  (prepare-ns! (str (u/ns-sym)))
  (let [guards (state/get :guards)
        spec (parse/parse form :guards guards)
        _ (state/swap! compiler/add-function spec)
        ;; Read back from state (add-function may enrich the spec)
        spec (clojure.core/get (state/get :functions) (:fullname spec))]
    (forms/declaration (current-compiler) spec)))

(u/defmac generic+
  "Add new cases to an existing generic.
   All given arities must already be known."
  [& form]
  (let [current-ns (u/ns-sym)]
    (prepare-ns! (str current-ns))
    (let [guards (state/get :guards)
          {:as extension-spec new-cases :cases}
          (parse/parse form :extension-ns current-ns :guards guards)

          ;; Resolve the fullname (may be an alias like one/foo → thetis.tries.one/foo)
          fullname (or (state/qualify-symbol (:fullname extension-spec))
                       (:fullname extension-spec))
          extension-spec (assoc extension-spec :fullname fullname)

          ;; Check if generic already had guard cases BEFORE this extension
          pre-spec (clojure.core/get (state/get :functions) fullname)
          had-guards? (some :guard (:cases pre-spec))

          _ (state/swap! compiler/extend-function extension-spec (str current-ns))
          extended-spec (clojure.core/get (state/get :functions) fullname)
          has-new-guards? (some :guard new-cases)
          ;; Re-emit function-definition only when adding first guard cases
          ;; to a generic that didn't have any (needs two-phase dispatch wrapper)
          needs-redefine? (and has-new-guards? (not had-guards?))]
      (forms/extension (current-compiler)
                       (assoc extended-spec :cases new-cases)
                       :redefine-fn (when needs-redefine? extended-spec)))))

(u/defmac implements?
  "Test if something implements one or several generics."
  [v & names]
  (let [specs (map get-spec! names)]
    (forms/implements-all? v specs)))

(u/defmac type+
  "Like extend-type — implement several generics for a given type."
  [tag & impls]
  `(do ~@(mapv #(forms/implement tag %) impls)))

(u/defmac thing
  "Like reify but for generics."
  [& impls]
  (let [c (current-compiler)]
    (forms/thing c
                 (map (fn [[name & cases]]
                        (cons (state/qualify-symbol name) cases))
                      impls))))

(u/defmac fork
  "Clone a generic function, optionally under a new name."
  ([name]
   `(fork nil ~name))
  ([new-name original-name]
   (prepare-ns! (str (u/ns-sym)))
   (let [new-name (or new-name (symbol (name original-name)))
         qualified-original (state/qualify-symbol original-name)
         _ (state/swap! compiler/clone-function qualified-original new-name)
         ;; clone generates fullname via name_derive (new-name + *ns*)
         fullname (symbol (str *ns*) (name new-name))
         spec (clojure.core/get (state/get :functions) fullname)]
     (forms/declaration (current-compiler) spec))))

(u/defmac fork+
  "Fork a generic and immediately extend it."
  [name original-name & impls]
  `(do (fork ~name ~original-name)
       (generic+ ~name ~@impls)))

(u/defmac register-type
  "Register a new type with classes, groups, and implementations."
  [tag & {:keys [classes groups impls]}]
  (let [current-ns (str (u/ns-sym))]
    (prepare-ns! current-ns)
    (assert (every? symbol? classes)
            "not a class")
    (assert (every? (state/get :types) groups)
            "unknown group")
    ;; Register type contribution atomically
    (state/swap! compiler/register-type-contribution current-ns tag classes groups)
    `(do ~@(mapv (partial forms/extend-class (current-compiler)) classes)
         (type+ ~tag ~@impls))))

(u/defmac deft
  "Define a record type with constructor, cast function, and group membership."
  [nam fields & body]
  (let [[groups impls]
        (if (= :belongs-to (first body))
          [(second body) (drop 2 body)]
          [nil body])

        ns-str (str *ns*)
        tag-name (name nam)
        qualified-tag (keyword ns-str tag-name)
        class-str (apply str (map str/capitalize (str/split tag-name #"-")))
        class-sym (symbol class-str)

        positional-constructor-sym (symbol (str "->" class-sym))
        map-constructor-sym (symbol (str "map->" class-sym))
        cast-fn-sym (symbol (str "->" nam))

        qualified-class
        (if (state/cljs?)
          (symbol ns-str (name class-sym))
          (u/sym (str/replace ns-str "-" "_") "." (name class-sym)))]

    `(do (defrecord ~class-sym ~fields)
         (def ~nam ~positional-constructor-sym)
         (defg ~cast-fn-sym [x#]
           ~qualified-tag x#
           :map (~map-constructor-sym x#))
         (register-type ~qualified-tag
                        {:classes [~qualified-class]
                         :groups ~groups
                         :impls ~(mapv (partial forms/deft_impl-bind-fields fields) impls)}))))

(comment
  (state/get :types))
