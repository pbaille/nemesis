(ns generics.impl.parse
  (:require [generics.impl.utils :as u :refer [pp]]))

(comment :doc

         ;; lexique

         ;; argv : argument vector
         ;; body : a list of expressions, where the last is the returned value and the other for side-effects only
         ;; arity : a list starting with an 'argv' followed by one or several expression constituing the 'body' of the arity

         ;; example :

         (fn foo
           "i'm the foo function"
           ([a]
            (do-something-with a))
           ([a b]
            (println "got " a b)
            (+ a b)))

         ;; the function foo has 2 arities
         ;; 1: ([a] (do-something-with a))
         ;; 2: ([a b] (println "got " a b) (+ a b))
         ;;    where :
         ;;      argv is [a b]
         ;;      body is ((println "got " a b) (+ a b))
         ;;      return expression is (+ a b)
         ;;      side-effect is (println "got " a b)

         ;; a generic function is like a regular function with slightly different bodies
         ;; the body of a generic function's arity is constituted of type-cases (later refered as 'cases')

         (fn mark-me
           ([a]
            :num [:num a]
            :vec [:vec a]
            [:unknown a]))

         ;; the 'mark-me' generic has only one arity
         ;; with 2 type-cases :
         ;; - :num [:num a]
         ;; - :vec [:vec a]
         ;; and a default case : [:unknown a]

         ;; as you can see this syntax do not support side-effects as the previous one does
         ;; but you can always use a 'do' block when needed


         ;; in this namespace the word 'parse' typically means that we are turning one expression or subexpression into a map
         )

(defn parse-form

  [{:as                generic
    [_ name fst & nxt] :form}]

  (let [[doc fst & nxt]
        (if (string? fst)
          (cons fst nxt)
          (concat ["" fst] nxt))

        [meta fst & nxt]
        (if (map? fst)
          (cons fst nxt)
          (concat [{} fst] nxt))

        arities
        (if (vector? fst)
          [(cons fst nxt)]
          (into [fst] nxt))]

    (assoc generic
      :meta meta
      :name name
      :doc doc
      :arities arities)))

(defn derive-name [{:as generic n :name}]
  (let [ns-str (or (namespace n) (str *ns*))
        ns (symbol ns-str)
        name-str (name n)
        name (symbol name-str)]
    (assoc generic
      :ns ns
      :name name
      :protocol-prefix (u/sym 'I (munge name))
      :method-prefix (u/sym 'p_ (munge name))
      :fullname (symbol ns-str name-str))))

(defn arity_with-names [{:as generic :keys [protocol-prefix method-prefix]}
                        {:as arity :keys [size]}]
  (letfn [(arify-name [n]
            (u/sym n '_ (str size)))]
    (assoc arity
      :protocol-name (arify-name protocol-prefix)
      :method-name (arify-name method-prefix)
      :argv (u/argv_litt size))))

(defn arity_parse-body
  {:ex '(-> (:num [:num a]
             :vec [:vec a]
             [:unknown a])

            {:cases   [{:type :num, :expr [:num a]}
                       {:type :vec, :expr [:vec a]}],
             :default {:expr [:unknown a]}})}
  [body]
  (letfn [(pair->case [[type expr]]
            {:type type :expr expr})
          (cases [xs]
            (map pair->case (partition 2 xs)))]
    (if (even? (count body))
      {:cases (cases body)}
      {:cases   (cases (butlast body))
       :default {:expr (last body)}})))

(defn parse-arity
  {:ex '(-> ([a] :num [:num a]
             :vec [:vec a]
             [:unknown a])

            {:size    1,
             :cases   [{:type :num, :expr [:num a], :argv [a]}
                       {:type :vec, :expr [:vec a], :argv [a]}],
             :default {:expr [:unknown a] :argv [a]}})}
  [[argv & body]]
  (let [variadic (u/argv_variadic? argv)
        {:keys [cases default]} (arity_parse-body body)
        with-argv (partial merge {:argv argv})]
    (merge
     {:size    (count argv)
      :cases   (mapv with-argv cases)
      :default (with-argv default)}
     (when variadic {:variadic variadic}))))

(defn parse-arities [generic]
  (update generic
          :arities
          #(->> (map parse-arity %)
                (map (partial arity_with-names generic))
                (map (juxt :size identity))
                (into {}))))

(defn parse [{:as generic :keys [env form]}]
  (-> generic
      parse-form
      derive-name
      parse-arities))



(parse {:form '(fn mark-me
                 ([a]
                  :num [:num a]
                  :vec [:vec a]
                  [:unknown a]))})

(comment

 (defn compile [{:keys [env form]}]
   (reverse (next form)))

 (defmacro m [& _]
   (compile (u/expansion-state)))

 (m 1 2 3 +)

 (defmacro defmac [name impl]
   `(defmacro ~name ~'[& _]
      (~impl (u/expansion-state))))

 (defmac M compile)

 (M 1 2 3 +))