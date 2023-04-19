(ns poly.utils.misc
  (:require [clojure.pprint :as pprint]))

(defn error [& xs]
  (throw (new Exception (apply str xs))))

(defn pp [& xs] (mapv pprint/pprint xs) (last xs))

(defn pretty-str [& xs] (with-out-str (apply pp xs)))

(defn $vals [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

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
