(ns thetis.utils.misc
  "General utilities for thetis: symbol manipulation, fn-case normalization,
   CLJS JS interop helpers, macro helpers (defmac), and binding pattern utilities."
  (:require [clojure.pprint :as pprint]
            [clojure.walk :as walk]))

(defn error [& xs]
  (throw (new Exception (apply str xs))))

(defn pp [& xs] (mapv pprint/pprint xs) (last xs))

(defn pretty-str [& xs] (with-out-str (apply pp xs)))

(do :cljs

    (defn cljs_prototype-assoc-form [obj meth impl]
      (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl)))

(defn $vals
  "Map a function over the values of a map, preserving keys."
  [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(do :symbols

    (defn ns-sym
      "Return the current namespace as a symbol."
      []
      (symbol (str *ns*)))

    (defn fullname
      "Return the fully-qualified name string of a symbol or keyword."
      [x]
      (if (string? x)
        x
        (if-let [ns (namespace x)]
          (str ns "/" (name x))
          (name x))))

    (defn sym
      "Construct a symbol by concatenating the fullnames of the given args."
      [& xs]
      (->> xs
           (remove nil?)
           (map fullname)
           (apply str)
           symbol))

    (defn with-ns
      "Qualify a symbol with the given namespace (or *ns* if not specified)."
      ([sym]
       (with-ns *ns* sym))
      ([ns sym]
       (symbol (str ns) (str sym)))))

(do :fn

    (defn argv_litt
      "Generate a literal argv vector of n gensyms."
      [n & [prefix]]
      (vec (repeatedly n #(gensym (or prefix "a_")))))

    (defn argv_variadic?
      "Does this argv contain a `&` rest parameter?"
      [x]
      (boolean ((set x) '&)))

    (defn argv_unvariadify
      "Remove `&` from a variadic argv, yielding a flat vector.
       [x y & zs] → [x y zs]"
      [argv]
      (-> argv butlast butlast
          vec (conj (last argv))))

    (defn argv_variadify
      "Insert `&` before the last element of a flat argv.
       [x y zs] → [x y & zs]"
      [v]
      (vec (concat (butlast v) ['& (last v)])))

    (defn fn-case_bodify
      "Normalize a fn case so it has exactly [pattern body], wrapping multiple body forms in `do`."
      [[pattern b1 & bs]]
      (list pattern (if-not bs b1 (list* 'do b1 bs))))

    (defn fn-cases_normalize
      "Normalize fn cases to a vector of [pattern body] pairs.
       Handles single-arity `[args body]` and multi-arity `((args body) ...)` forms."
      [xs]
      (mapv fn-case_bodify
            (cond
              (vector? (first xs)) [xs]
              (every? seq? xs) (vec xs)
              :else (error "invalid fn cases:\n " xs))))

    (defn binding-pattern_ensure-top-level-sym

      "this checks that the given pattern has a top level binding
       for symbols it is itself
       for destructuring patterns it is handle via the :as syntax
       if not present the given `default-sym` will be inserted

       we will return a tuple of the form [top-lvl-sym pattern]"

      [pat default-sym]

      (cond (symbol? pat)
            [pat pat]

            (vector? pat)
            (if (some #{:as} pat)
              [(last pat) pat]
              [default-sym (vec (concat pat [:as default-sym]))])

            (map? pat)
            (if (contains? pat :as)
              [(:as pat) pat]
              [default-sym (assoc pat :as default-sym)])))

    (defn parse-fn
      "Parse a defn-like form into {:name, :doc, :meta, :impls, :cases}.
       Used by `defmac` to extract the structure of macro definitions."
      [[fst & nxt :as all]]

      (let [[name fst & nxt]
            (if (symbol? fst)
              (cons fst nxt)
              (concat [nil fst] nxt))

            [doc fst & nxt]
            (if (string? fst)
              (cons fst nxt)
              (concat ["" fst] nxt))

            [meta fst & nxt]
            (if (map? fst)
              (cons fst nxt)
              (concat [{} fst] nxt))

            impls
            (if (vector? fst)
              {fst (vec nxt)}
              (into {}
                    (map
                      (fn [[args & body]]
                        [args (vec body)])
                      (cons fst nxt))))]

        {:meta meta
         :name (or name (gensym))
         :name? name
         :doc doc
         :impls impls
         :cases (mapv (partial apply list*) impls)})))

(do :macro-helpers

    (defn doall-rec
      "Realize all nested potentially lazy sequences.
       Critical for macros using dynamic var-based expansion state —
       lazy sequences would escape the dynamic binding scope."
      [x]
      (cond (seq? x) (or (seq (map doall-rec x)) ())
            (map? x) (into {} (map (fn [[k v]] [(doall-rec k) (doall-rec v)])) x)
            (set? x) (into #{} (map doall-rec) x)
            (vector? x) (mapv doall-rec x)
            (coll? x) (into (empty x) (map doall-rec) x)
            :else x))

    (defmacro defmac
      "Define a macro with automatic expansion state management.
       Generates:
       1. A companion function (`name-fn`) for REPL use with quoted args
       2. The macro itself, wrapped in `thetis.state/expanding` + `doall-rec`
       3. A partial-apply var (`name-fn*`) for programmatic use

       The `expanding` wrapper binds `&env` and `&form` into dynamic state.
       The `doall-rec` wrapper forces all lazy sequences within the dynamic scope.

       Usage: (defmac name doc? [args] body...)"
      [& body]
      (when-not (:ns &env) ;; defmac emits nil in cljs
        (let [body (walk/postwalk-replace '{&env (thetis.state/env) &form (thetis.state/form)} body)
              {:keys [name doc meta cases]} (parse-fn body)
              fname (sym name '-fn)
              fname* (sym fname '*)]
          `(do
             (defn ~fname ~@cases)
             (def ~fname* (partial apply ~fname))
             (defmacro ~name ~doc
               ~(assoc meta :fn fname)
               ~@(mapv (fn [[argv & body]]
                         `(~argv (thetis.state/expanding
                                   (doall-rec
                                     (do ~@body)))))
                       cases)))))))
