(ns thetis.compiler.forms
  "Code generation for thetis generics.
   Emits protocol declarations, extend calls, function definitions,
   prototype registrations, and guard dispatch wrappers.
   Handles both CLJ (via extend) and CLJS (via prototype assignment)."
  (:require [thetis.utils.misc :as u]
            [thetis.utils.names :as names]
            [thetis.utils.expansion :as expansion]
            [thetis.compiler.core :as compiler]
            [thetis.types :as types]
            [clojure.core :as c]))

(declare function-definition)

(do :prototypes

    (def ^:dynamic *prototypes-sym*
      "Symbol pointing to the runtime prototypes atom.
       Dynamically rebindable for code generation in `thing`/`fork` scenarios
       where the target atom differs from `thetis.core/prototypes`."
      'thetis.core/prototypes)

    (defn prototype_registering
      "Emit code to register a single case's implementation in the prototypes atom.
       The prototype is stored as {type → {fullname → {arity → (fn compiled)}}}."
      [{:as _spec :keys [fullname cloned-from]}
       {:as _case :keys [cloned compiled arity type]}]
      (let [psym *prototypes-sym*
            form (fn [type]
                   `(swap! ~psym
                           assoc-in
                           [~type '~fullname ~arity]
                           ~(if cloned
                              `(get-in (deref ~psym) [~type '~cloned-from ~arity])
                              `(fn ~compiled))))]
        (if (set? type)
          `(do ~@(mapv form type))
          (form type))))

    (defn prototypes_registering
      "Emit code to register all non-guard cases from a spec into the prototypes atom."
      [spec]
      (->> (:cases spec)
           (remove :guard)  ;; guard cases go through guard-impls, not prototypes
           (mapv (partial prototype_registering spec))
           (list* 'do))))

(do :guards

    (def ^:dynamic *guard-impls-sym*
      "Symbol pointing to the runtime guard-impls atom.
       Shape: {generic-fullname → arity → [{:pred fn :impl fn} ...]}."
      'thetis.core/guard-impls)

    (defn guard_registering
      "Emit code to register a guard case's pred+impl in the guard-impls atom.
       Each guard case registers as {:pred (fn [x] (and base-pred guard-pred)) :impl (fn [args] body)}."
      [{:as spec :keys [fullname]}
       {:as guard-case :keys [guard-spec compiled arity]}]
      (let [{:keys [base pred]} guard-spec
            gsym (gensym "x_")
            base-pred (types/symbolic-pred-body (types/get-reg) base gsym)
            combined-pred `(fn [~gsym] (c/and ~base-pred (~pred ~gsym)))
            impl-fn `(fn ~compiled)]
        `(swap! ~*guard-impls-sym*
                update-in ['~fullname ~arity]
                (fnil conj [])
                {:pred ~combined-pred
                 :impl ~impl-fn})))

    (defn guards_registering
      "Emit registration code for all guard cases in a spec.
       Returns nil if spec has no guard cases."
      [spec]
      (let [guard-cases (filter :guard (:cases spec))]
        (when (seq guard-cases)
          `(do ~@(mapv (partial guard_registering spec) guard-cases))))))

(do :extension

    (do :cljs-extend

        (let [cljs-base-type
              {nil       "null"
               'object   "object"
               'string   "string"
               'number   "number"
               'array    "array"
               'function "function"
               'boolean  "boolean"
               'default  "_"}]

          (letfn [(protocol-prefix [psym]
                    (str (-> (str psym)
                             (.replace \. \$)
                             (.replace \/ \$))
                         "$"))

                  (cljs-extend_properties
                    [protocol-name method-name arity]
                    (let [psym (symbol protocol-name)
                          ns (.replace (namespace psym) \- \_)
                          name (name psym)
                          prefix (protocol-prefix (symbol ns name))]
                      {:sentinel prefix
                       :method   (str prefix (munge method-name) "$arity$" arity)}))]

            (defn cljs-extend1 [class protocol method arity impl]
              (if-let [class-str (cljs-base-type class)]
                `(do (cljs.core/unchecked-set ~protocol ~class-str true)
                     (cljs.core/unchecked-set ~(u/with-ns (namespace protocol) method) ~class-str ~impl))
                (let [props (cljs-extend_properties protocol method arity)]
                  `(do ~(u/cljs_prototype-assoc-form class (:sentinel props) 'cljs.core/PROTOCOL_SENTINEL)
                       ~(u/cljs_prototype-assoc-form class (:method props) impl))))))))

    (defn extension-form
      "Emit a single extend call for one class/arity/protocol combination.
       Handles CLJ (via clojure.core/extend) and CLJS (via prototype assignment) differently."
      [{:as _compiler :keys [expansion]}
       {:as _spec :keys [ns fullname]}
       {:as _case :keys [class arity type protocol-name method-name]}]
      (let [psym *prototypes-sym*
            type (if (set? type) (first type) type)
            impl `(get-in (deref ~psym) [~type '~fullname ~arity])
            qualified-protocol (u/with-ns ns protocol-name)]

        (if (expansion/cljs? expansion)

          (cljs-extend1 class qualified-protocol
                        method-name arity impl)
          (list 'do
                (when class (list `c/import (list 'quote class)))
                (list `c/extend class
                      qualified-protocol
                      {(keyword method-name) impl})))))

    (defn extend-class
      "Emit extend calls for all generics that have implementations for the given class."
      [compiler class]
      `(do ~@(reduce (fn [ret {:keys [spec cases]}]
                       (into ret (mapv (partial extension-form compiler spec)
                                       cases)))
                     [] (compiler/get-class-cases compiler class))))

    (defn protocol-extension
      "Emit extend calls for all class/arity combinations in a spec."
      [compiler spec]
      `(do ~@(mapv (partial extension-form compiler spec)
                   (compiler/class-extensions compiler spec))))

    (defn extension
      "Top-level code generation for `generic+`.
       Registers prototypes, guard impls, optionally re-emits the function definition
       (when adding first guard cases), and extends protocols."
      [compiler {:as spec :keys [cases]} & {:keys [redefine-fn]}]
      `(do
         ~(prototypes_registering spec)
         ~@(when-let [gf (guards_registering spec)] [gf])
         ~@(when redefine-fn
             [(function-definition redefine-fn)])
         ~(protocol-extension compiler spec))))

(do :declaration

    (defn protocol-declaration
      "Emit defprotocol forms — one protocol per arity."
      [{:keys [arities ns]}]
      `(do ~@(mapv (fn [[_ {:keys [protocol-name method-name argv]}]]
                     `(defprotocol ~(u/with-ns ns protocol-name)
                        ~(list method-name argv)))
                   arities)))

    (defn- guard-cases-for-arity
      "Extract guard cases for a specific arity from a spec."
      [spec arity]
      (filter :guard
              (filter #(= arity (:arity %))
                      (:cases spec))))

    (defn- has-guard-cases?
      "Does this spec have any guard cases?"
      [spec]
      (some :guard (:cases spec)))

    (defn function-definition
      "Generate the defn form for a generic.
       When guard cases are present, emits a dispatch wrapper that checks
       the runtime guard-impls atom before falling through to protocol dispatch.
       This allows guard cases to be added via generic+ (open extension)."
      [{:as spec :keys [name arities cases fullname]}]
      (let [has-guards? (has-guard-cases? spec)
            guard-sym 'thetis.core/guard-impls]
        `(defn ~name
           ~@(mapv (fn [[arity-key {:keys [variadic argv method-name]}]]
                     (let [user-argv (if variadic (u/argv_variadify argv) argv)
                           protocol-call `(~method-name ~@argv)]
                       (if has-guards?
                         ;; Two-phase dispatch: check guard-impls atom, then protocol
                         (list user-argv
                               `(let [guards# (get-in (deref ~guard-sym) ['~fullname ~arity-key])]
                                  (if guards#
                                    (or (c/some (fn [g#] (when ((:pred g#) ~(first argv))
                                                           (apply (:impl g#) ~(vec argv))))
                                                guards#)
                                        ~protocol-call)
                                    ~protocol-call)))
                         ;; No guards — pure protocol dispatch (status quo)
                         (list user-argv protocol-call))))
                   arities))))

    (defn cleaning
      "Emit ns-unmap calls to clean up any previous protocol/method/fn defs
       for this generic. Ensures redefinition is idempotent."
      [{:keys [ns name arities]}]
      `(do
         ~@(mapv (fn [x#] `(ns-unmap '~(symbol ns) '~x#))
                 (cons name (mapcat (juxt :method-name :protocol-name) (vals arities))))))

    (defn declaration
      "Top-level code generation for `defg`.
       Cleans previous defs, declares protocols, defines the dispatch function,
       registers prototypes/guards, and extends protocols. Returns the generic's name."
      [compiler spec]
      `(do ~(cleaning spec)
           ~(protocol-declaration spec)
           ~(function-definition spec)
           ~(prototypes_registering spec)
           ~@(when-let [gf (guards_registering spec)] [gf])
           ~(protocol-extension compiler spec)
           ~(:name spec))))

(do :implement

    (def ^:dynamic *generic+-sym*
      "Symbol pointing to the `generic+` macro.
       Rebindable for `type+`/`implement` code generation."
      'thetis.core/generic+)

    (defn implements-all? [expr specs]
      (if (symbol? expr)
        `(and ~@(mapv (fn [spec]
                        `(when (or ~@(mapv (fn [protocol-name] `(satisfies? ~(u/with-ns (:ns spec) protocol-name) ~expr))
                                           (map (comp :protocol-name val) (:arities spec))))
                           ~expr))
                      specs))
        (let [vsym (gensym)]
          `(let [~vsym ~expr]
             ~(implements-all? vsym specs)))))

    (defn implement_impl-body->cases
      "Transform a type+ implementation body into generic+ case triples [pattern type body]."
      [tag cases]
      (mapv (fn [[pat bod]] (list pat tag bod))
            (u/fn-cases_normalize cases)))

    (defn implement
      "Emit a generic+ call for a single generic within a type+ block."
      [tag [name & body]]
      `(~*generic+-sym*
        ~name
        ~@(implement_impl-body->cases tag body))))

(do :thing

    (defn thing_parse-impl-cases
      "Parse a single generic's cases within a `thing` block.
       Resolves protocol/method names from the compiler's function registry."
      [compiler [name & cases]]
      (let [{:as _spec :keys [ns method-prefix protocol-prefix]}
            (compiler/get-function! compiler name)
            with-clean-pattern
            (fn [x] (update x :pattern (comp vec (partial remove #{'&}))))
            with-variadic-flag
            (fn [x] (assoc x :variadic (u/argv_variadic? (:pattern x))))
            qualify
            #(symbol (c/name ns) (c/name %))
            with-names
            (fn [x]
              (let [ari (count (:pattern x))]
                (assoc x :arity ari
                         :method-name (qualify (names/name_arify method-prefix ari))
                         :protocol-name (qualify (names/name_arify protocol-prefix ari)))))]

        (->> (u/fn-cases_normalize cases)
             (map (fn [[pat bod]] {:pattern pat :body bod}))
             (map with-variadic-flag)
             (map with-clean-pattern)
             (map with-names))))

    (defn thing_cases->decls
      "Transform parsed thing cases into protocol+method pairs for reify."
      [xs]
      (mapcat (fn [{:keys [method-name protocol-name body pattern]}]
                [protocol-name (list method-name pattern body)])
              xs))

    (defn thing
      "Emit a reify form implementing the given generics."
      [compiler impls]
      `(reify
         ~@(mapcat thing_cases->decls
                   (map (partial thing_parse-impl-cases compiler) impls)))))

(do :type-extension

    (defn deft_impl-bind-fields
      "Transform a deft implementation body to destructure the record fields
       from the first argument."
      [fields [name argv & body]]
      (let [[this-sym this-pat]
            (u/binding-pattern_ensure-top-level-sym (first argv) (gensym))]
        `(~name ~(vec (cons this-pat (next argv)))
          (let [{:keys ~fields} ~this-sym] ~@body))))
    )
