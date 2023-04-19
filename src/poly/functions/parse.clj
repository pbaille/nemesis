(ns poly.functions.parse
  (:require [poly.utils.misc :as u]))

(defn normalize-body
  "normalize the body of a generic declaration
   e.g: the part after the name and the docstring"
  [xs]
  (if (vector? (first xs))
    (list xs)
    xs))


(defn parse-arity-body [body]
  (letfn [(pairs [xs]
            (vec (partition 2 xs)))]
    (if (even? (count body))
      {:cases (pairs body)}
      {:default true
       :cases (conj (pairs (butlast body))
                    [:default (last body)])})))


(defn arity
  [[argv & body]]
  (let [variadic (u/argv_variadic? argv)
        argv (if variadic (u/argv_unvariadify argv) argv)]
    (merge
     (parse-arity-body body)
     {:arity (count argv)
      :argv argv
      :variadic variadic})))

(defn arity-names
  [arity {:as _names :keys [protocol-prefix method-prefix]}]
  {:protocol-name (u/name_arify protocol-prefix arity)
   :method-name (u/name_arify method-prefix arity)})


(defn arities [body]
  (map arity
       (normalize-body body)))


(defn arities->cases
  [parsed-arities]
  (mapcat (fn [{:as a :keys [cases]}]
            (let [case0 (dissoc a :cases :default)]
              (reduce
               (fn [cases [t e]]
                 (conj cases (assoc case0 :type t :expr e)))
               [] cases)))
          parsed-arities))


(defn arity-map
  [names arities]
  (->> (group-by :arity arities)
       (map (fn [[arity xs]]

              (assert (<= (count (filter :default xs)) 1)
                      (str "more than one default case for arity " arity))
              (assert (apply = (map :variadic xs))
                      (str `arity-map ": inconsistent arity " arity "\n" xs))

              (let [argv-litt (u/argv_litt arity)]
                [arity (merge (arity-names arity names)
                              {:argv argv-litt
                               :variadic (:variadic (first xs))
                               :default (some :default xs)})])))
       (into {})))


(defn compile-cases

  [{:as spec :keys [ns cases]}
   {:keys [lambda-case-compiler extension-ns]}]
  (let [compile-case (or lambda-case-compiler identity)
        extend-case (fn [{:keys [argv expr] :as case}]
                      (assoc case
                             :ns (or extension-ns ns)
                             :compiled (compile-case (list argv expr))))]
    (assoc spec
           :cases
           (mapv extend-case cases))))


(defn parse
  ([form] (parse form {}))
  ([[name & body]
    & {:as options :keys [extension-ns lambda-case-compiler]}]
   (let [doc (when (string? (first body)) (first body))
         body (if doc (rest body) body)
         names (u/name_derive name)
         arities (arities body)
         cases (arities->cases arities)
         arity-map (arity-map names arities)
         variadic (boolean (some :variadic arities))]

     (compile-cases
      (merge names
             {:doc doc
              :cases cases
              :variadic variadic
              :arities arity-map})
      options))))

(comment :scratch
         (arities '([x] :yo))
         (arities '([x] :vec :yovec :yo))

         (parse '(yo "doc" [x] :vec :yovec :yo))

         (parse '(g1 [x]
                     ;; prim type impl
                     :vec "I am vec"
                     ;; this type is a group
                     ;; under the hood it implements for all collections
                     :coll ["I am coll" x]
                     ;; group litteral can be handy
                     #{:key :sym} "I am key-or-sym"

                     "Who am I ?"))
         (parse '(yo "doc" [x] :vec :yovec))
         (parse '(yo [x] :yo)))
