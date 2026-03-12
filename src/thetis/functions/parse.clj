(ns thetis.functions.parse
  "Parser for defg and generic+ forms.
   Transforms the user-facing syntax into structured spec maps
   with arities, cases, and compiled implementations."
  (:require [thetis.utils.misc :as u]
            [thetis.utils.names :as names]))

(defn normalize-body
  "normalize the body of a generic declaration
   e.g: the part after the name and the docstring"
  [xs]
  (if (vector? (first xs))
    (list xs)
    xs))


(defn parse-arity-body
  "Parse the body of a single arity into {:cases [...] :default bool}.
   Even-count bodies are all type/expr pairs.
   Odd-count bodies have a trailing default expression."
  [body]
  (letfn [(pairs [xs]
            (vec (partition 2 xs)))]
    (if (even? (count body))
      {:cases (pairs body)}
      {:default true
       :cases (conj (pairs (butlast body))
                    [:default (last body)])})))


(defn arity
  "Parse a single arity clause [argv & body] into a structured map
   with :arity, :argv, :variadic, :cases, and :default."
  [[argv & body]]
  (let [variadic (u/argv_variadic? argv)
        argv (if variadic (u/argv_unvariadify argv) argv)]
    (merge
     (parse-arity-body body)
     {:arity (count argv)
      :argv argv
      :variadic variadic})))

(defn arity-names
  "Derive protocol and method names for a given arity count."
  [arity {:as _names :keys [protocol-prefix method-prefix]}]
  {:protocol-name (names/name_arify protocol-prefix arity)
   :method-name (names/name_arify method-prefix arity)})


(defn arities
  "Parse all arity clauses from a normalized body."
  [body]
  (map arity
       (normalize-body body)))


(defn arities->cases
  "Flatten parsed arities into a flat list of case maps,
   each with :type, :expr, :arity, :argv, :variadic."
  [parsed-arities]
  (mapcat (fn [{:as a :keys [cases]}]
            (let [case0 (dissoc a :cases :default)]
              (reduce
               (fn [cases [t e]]
                 (conj cases (assoc case0 :type t :expr e)))
               [] cases)))
          parsed-arities))


(defn arity-map
  "Build the {arity → arity-info} map from parsed arities.
   Validates single default per arity and consistent variadic flags."
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
  "Compile case expressions into [argv expr] forms and annotate guard cases.
   Optionally applies a lambda-case-compiler transform."
  [{:as spec :keys [ns cases]}
   {:keys [lambda-case-compiler extension-ns guards]}]
  (let [compile-case (or lambda-case-compiler identity)
        extend-case (fn [{:keys [argv expr type] :as case}]
                      (let [guard? (and guards
                                        (keyword? type)
                                        (contains? guards type))]
                        (cond-> (assoc case
                                       :ns (or extension-ns ns)
                                       :compiled (compile-case (list argv expr)))
                          guard? (assoc :guard true
                                        :guard-spec (get guards type)))))]
    (assoc spec
           :cases
           (mapv extend-case cases))))


(defn parse
  "Parse a defg or generic+ form into a complete spec map.
   Extracts name, docstring, mode metadata, arities, and cases.
   Options: :extension-ns, :lambda-case-compiler, :guards."
  ([form] (parse form {}))
  ([[name & body]
    & {:as options :keys [extension-ns lambda-case-compiler guards]}]
   (let [name-meta (meta name)
         mode (or (:mode name-meta) :refine)
         doc (when (string? (first body)) (first body))
         body (if doc (rest body) body)
         names (names/name_derive name)
         arities (arities body)
         cases (arities->cases arities)
         arity-map (arity-map names arities)
         variadic (boolean (some :variadic arities))]

     (compile-cases
      (merge names
             {:doc doc
              :mode mode
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
