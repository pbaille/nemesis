(ns nemesis.impl.utils
  (:refer-clojure :exclude [empty])
  (:require [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [nemesis.state :as state]))

(defn error [& xs]
  (throw (new Exception (apply str xs))))

(defn error-form [& xs]
  `(throw (new ~(if (state/cljs?) 'js/Error 'Exception) (str ~@xs))))

(defn pp [& xs] (mapv pp/pprint xs) nil)

(defn pretty-str [& xs] (with-out-str (apply pp xs)))

(def kset (comp set keys))

(do :symbols

    (defn ns-sym []
      (symbol (str *ns*)))

    (defn fullname [x]
      (if (string? x)
        x
        (if-let [ns (namespace x)]
          (str ns "/" (name x))
          (name x))))

    (defn sym [& xs]
      (->> xs
           (remove nil?)
           (map fullname)
           (apply str)
           symbol))

    (defn with-ns
      ([sym]
       (with-ns *ns* sym))
      ([ns sym]
       (symbol (str ns) (str sym)))))

(do :domain-specific

    (defn name_derive [n]
      (let [ns-str (or (namespace n) (str *ns*))
            ns (symbol ns-str)
            name-str (name n)
            name (symbol name-str)]
        {:ns ns
         :name name
         :protocol-prefix (sym 'I (munge name))
         :method-prefix (sym 'p_ (munge name))
         :fullname (symbol ns-str name-str)}))

    (defn name_arify [n a]
      (sym n '_ (str a))))

(do :fn

    (defn argv_litt [n & [prefix]]
      (vec (repeatedly n #(gensym (or prefix "a_")))))

    (defn argv_variadic? [x]
      (boolean ((set x) '&)))

    (defn argv_unvariadify [argv]
      (-> argv butlast butlast
          vec (conj (last argv))))

    (defn argv_variadify [v]
      (vec (concat (butlast v) ['& (last v)])))

    (defn fn-case_bodify [[pattern b1 & bs]]
      (list pattern (if-not bs b1 (list* 'do b1 bs))))

    (defn fn-cases_normalize [xs]
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

    (defn parse-fn [[fst & nxt :as all]]

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

(defn map_diff
  "return m1 minus the entries (key and val) that are in m2"
  [m1 m2]
  (into {} (clojure.set/difference (set m1) (set m2))))

(do :cljs

    (defmacro cljs_prototype-assoc [obj meth impl]
      (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))

    (defn cljs_prototype-assoc-form [obj meth impl]
      (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl)))

(do :$

    (defn $vals [m f]
      (into {} (map (fn [[k v]] [k (f v)]) m)))

    (defn empty [x]
      (cond
        (record? x) (apply dissoc x (keys x))
        (map-entry? x) []
        :else (clojure.core/empty x)))

    (defn $fn [ffn]
      (fn [x f]
        (if (seq? x)
          (ffn f x)
          (into (empty x) (ffn f x)))))

    (def shrink+ ($fn filter))
    (def shrink- ($fn remove))
    (def $! ($fn keep))
    (def $ ($fn map))

    (defn doall-rec
      "realize all nested potetially nested lazy sequences
       usefull in macros, because when using dynamic vars based expansion state, we have to be sure that there is no lazyness in the expansion
       otherwise dynamic vars will not be bounded as intended when expansion lazy parts are realized"
      [x]
      (cond (seq? x) (or (seq x) ())
            (coll? x) ($ x doall-rec)
            :else x))

    (defn findeep [x p]
      (cond
        (p x) (list x)
        (coll? x) (mapcat #(findeep % p) x)
        :else ())))


(defmacro defmac
  "define a regular macro
   but also a function that do the same thing as the macro (when receiving quoted args)
   note that if used from clojurescript, body have to contain only functions that are defined both in clojure and clojurescript"
  [& body]
  (when-not (:ns &env) ;; defmac emits nil in cljs
    (let [body
          (walk/postwalk-replace '{&env (state/env) &form (state/form)} body)

          {:keys [name doc meta cases]} (parse-fn body)
          fname (sym name '-fn)
          fname* (sym fname '*)]
      #_(clojure.pprint/pprint body)
      `(do
         (defn ~fname ~@cases)
         (def ~fname* (partial apply ~fname))
         (defmacro ~name ~doc
           ~(assoc meta :fn fname)
           ~@(mapv (fn [[argv & body]]
                     `(~argv (state/expanding
                               (doall-rec
                                 (do ~@body)))))
                   cases))))))