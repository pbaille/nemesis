(ns nemesis.impl.parse
  (:require [nemesis.impl.utils :as u]
            [nemesis.state :as state]))

(do :parse


    (defn no-implementation-error-form [name argv]
      (let [syms (disj (set (u/findeep argv symbol?)) '&)
            quot (partial list 'quote)]
        (u/error-form "missing implementation for generic: " (quot name)
                      "\n  pattern:\n  " (quot argv)
                      "\n  where:\n  " (zipmap (map quot syms)
                                               syms))))


    (defn normalize-body
      "normalize the body of a generic declaration
       e.g: the part after the name and the docstring"
      [xs]
      (if (vector? (first xs))
        (list xs)
        xs))


    (defn arity-default-expr
      [name argv couples]
      (if (odd? (count couples))
        (last couples)
        (no-implementation-error-form name argv)))

    (defn parse-arity-body [body]
      (letfn [(pairs [xs]
                (reverse (partition 2 xs)))]
        (if (even? (count body))
          {:cases (pairs body)}
          {:default (last body)
           :cases (pairs (butlast body))})))


    (defn arity
      [[argv & body]]
      (let [variadic (u/argv_variadic? argv)
            argv (if variadic (u/argv_unvariadify argv) argv)]
        (merge
         (parse-arity-body body)
         {:arity (count argv)
          :argv argv
          :variadic variadic})))


    (defn arities->cases
      [parsed-arities]
      (mapcat (fn [{:as a :keys [cases default]}]
                (let [case0 (dissoc a :cases :default)]
                  (reduce
                         (fn [cases [t e]]
                           (conj cases (assoc case0 :type t :expr e)))
                         [] (if default (cons [:default default] cases) cases))))
              parsed-arities))


    (defn arity-map
      [{:as _names :keys [protocol-prefix method-prefix]}
       arities]
      (->> (group-by :arity arities)
           (map (fn [[arity xs]]
                  (assert (apply = (map :variadic xs))
                          (str `arity-map ": inconsistent arity " arity "\n" xs))
                  (let [argv-litt (u/argv_litt arity)]
                    [arity (merge {:protocol-name (u/name_arify protocol-prefix arity)
                                   :method-name (u/name_arify method-prefix arity)
                                   :argv argv-litt
                                   :variadic (:variadic (first xs))}
                                  (if-some [{:keys [default argv]} (first (filter :default xs))]
                                    {:default {:expr default :argv argv}}))])))
           (into {})))


    (defn cases-summary [cases]
      (assert (if-let [varities (seq (map :arity (filter :variadic cases)))]
                (apply = varities)
                true)
              "several variadic arities with different length")
      (merge {:arity->default (into {} (map (juxt :arity :default) cases))}
             (when (boolean (some :variadic cases))
               {:variadic true})))


    (defn arities [body]
      (map arity
           (normalize-body body)))

    (defn compile-cases

      [{:as spec :keys [name ns cases arities]}
       {:keys [lambda-case-compiler
               extension-ns]}]
      (let [compile-case (or lambda-case-compiler @state/lambda-case-compiler*)
            extend-case (fn [{:keys [argv expr] :as case}]
                          (assoc case
                                 :ns ns
                                 :compiled (compile-case (list argv expr))))]
        (assoc spec
               :cases
               (mapv extend-case cases))))


    (defn parse
      ([form] (parse form {}))
      ([[name & body]
        {:as options :keys [extension-ns lambda-case-compiler ad-hoc]}]
       (let [doc (when (string? (first body)) (first body))
             body (if doc (rest body) body)
             names (u/name_derive name)
             arities (arities body)
             cases (arities->cases arities)
             arity-map (arity-map names arities)
             variadic (boolean (some :variadic arities))]

         (if ad-hoc
           (assert (not (some :default (vals arity-map)))
                   (str "default case can only be defined on generic creation (nemesis.core/defg)"
                        arity-map)))

         (compile-cases
           (merge names
                  {:doc doc
                   :cases cases
                   :variadic variadic
                   :arities arity-map})
           options))))

    )
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
