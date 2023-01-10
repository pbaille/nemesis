(ns nemesis.core
  (:require [nemesis.state :as state]
            [nemesis.types :as t]
            [nemesis.impl.forms :as forms]
            [nemesis.impl.registry :as reg]
            [nemesis.impl.parse :as parse]
            [nemesis.impl.utils :as u]
            [clojure.string :as str]))


(defonce prototypes (atom {}))


(u/defmac defg
  "create a generic function"
  [& form]
  (let [spec (parse/parse form)]
    (reg/register-spec! spec)
    (forms/declaration spec)))


(u/defmac generic+
  "add new cases to an existant generic
   all given arities must already be known"
  [& form]
  (let [{:as extension-spec new-cases :cases}
        (parse/parse form :extension-ns (u/ns-sym))
        extended-spec (reg/extend-spec! extension-spec)]
    (forms/extension
     (assoc extended-spec :cases new-cases))))


(u/defmac implements?
  "test if something implements one or several generics"
  [v & names]
  (let [specs (map reg/get-spec! names)]
    (forms/implements-all? v specs)))


(u/defmac type+
  "like extend type"
  [tag & impls]
  `(do ~@(mapv #(forms/implement tag %) impls)))


(u/defmac thing
  "like reify but for generics"
  [& impls]
  (forms/thing impls))


(u/defmac fork
  ([name]
   `(fork nil ~name))
  ([new-name original-name]
   (let [new-name (or new-name (symbol (name original-name)))
         spec (reg/clone-spec! original-name new-name)]
     (forms/declaration spec))))


(u/defmac fork+
  [name original-name & impls]
  `(do (fork ~name ~original-name)
       (generic+ ~name ~@impls)))


(u/defmac register-type
  [tag & {:keys [classes groups impls]}]
  (assert (every? symbol? classes)
          "not a class")
  (assert (every? (state/get :types) groups)
          "unknown group")
  (state/swap! update :types
               (fn [types]
                 (reduce (fn [types group] (update types group conj tag))
                         (assoc types tag classes) groups)))
  `(do ~@(map forms/extend-class classes)
       (type+ ~tag ~@impls)))


(u/defmac defrec [nam fields & body]
  (let [[groups impls]
        (if (= :belongs-to (first body))
          [(second body) (drop 2 body)]
          [nil body])

         ns-str (str *ns*)
         tag-name (name nam)
         class-str (apply str (map str/capitalize (str/split tag-name #"-")))
         class-sym (symbol class-str)

         qualified-class
         (if (state/cljs?)
           (symbol ns-str (name class-sym))
           (u/sym (str/replace ns-str "-" "_") "." (name class-sym)))]

    `(do (defrecord ~class-sym ~fields)
         (register-type ~(keyword ns-str tag-name)
                        {:classes [~qualified-class]
                         :groups ~groups
                         :impls ~(mapv (partial forms/deft_impl-bind-fields fields) impls)}))))

(comment
  (state/get :types))
