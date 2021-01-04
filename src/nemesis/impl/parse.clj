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


    (defn arity
      [name [argv & couples]]
      (let [variadic (u/argv_variadic? argv)
            argv (if variadic (u/argv_unvariadify argv) argv)
            default (arity-default-expr name argv couples)]
        (merge
          {:arity (count argv)
           :argv argv
           :cases (reverse (partition 2 couples))
           :default default}
          (when variadic {:variadic variadic}))))


    (defn arities->cases
      [parsed-arities]
      (mapcat (fn [{:as a :keys [cases default]}]
                (let [a (dissoc a :cases :default)]
                  (reduce
                    (fn [cases [t e]]
                      (conj cases
                            (assoc a :type t :expr e)))
                    [(assoc a :type :any
                              :expr default
                              :default true)]
                    cases)))
              parsed-arities))


    (defn arity-map
      [{:as _names :keys [protocol-prefix method-prefix]}
       arities]
      (->> arities
           (map (fn [arity]
                  [arity {:protocol-name (u/name_arify protocol-prefix arity)
                          :method-name (u/name_arify method-prefix arity)
                          :argv (u/argv_litt arity)}]))
           (into {})))


    (defn cases-summary [cases]
      (assert (if-let [varities (seq (map :arity (filter :variadic cases)))]
                (apply = varities)
                true)
              "several variadic arities with different length")
      (merge {:arities (set (map :arity cases))}
             (when (boolean (some :variadic cases))
               {:variadic true})))


    (defn body->cases [name body]
      (arities->cases
        (map (partial arity name)
             (normalize-body body))))


    (defn compile-cases

      [{:as spec :keys [name ns cases]}
       {:keys [lambda-case-compiler
               extension-ns]}]

      (assoc spec
        :cases
        (mapv (fn [{:keys [arity type argv expr] :as case}]
                (let [compile-case (or lambda-case-compiler @state/lambda-case-compiler*)]
                  (assoc case
                    :ns ns
                    :compiled (compile-case (list argv expr)))))
              cases)))


    (defn parse
      ([form] (parse form {}))
      ([[name & body]
        {:as options :keys [extension-ns lambda-case-compiler]}]
       (let [doc (when (string? (first body)) (first body))
             body (if doc (rest body) body)
             names (u/name_derive name)
             cases (body->cases name body)
             cases-summary (cases-summary cases)
             arity-map (arity-map names (:arities cases-summary))]

         (compile-cases
           (merge names
                  cases-summary
                  {:doc doc
                   :cases cases
                   :arities arity-map})
           options))))

    )