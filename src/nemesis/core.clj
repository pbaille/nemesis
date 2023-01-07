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
  (forms/declaration (parse/parse form)))

(u/defmac generic+
  "add new cases to an existant generic
   all given arities must already be known"
  [name & cases]
  (forms/extension
   (parse/parse (cons (state/qualify-symbol name) cases)
                {:extension-ns (u/ns-sym)})))

(u/defmac implements?
  "test if something implements one or several generics"
  ([v name]
   (let [gspec (reg/get-spec! name)
         vsym (gensym)]
     `(let [~vsym ~v]
        (when (or ~@(mapv (fn [protocol-name] `(satisfies? ~(u/with-ns (:ns gspec) protocol-name) ~vsym))
                          (map (comp :protocol-name val) (:arities gspec))))
          ~vsym))))
  ([v name & names]
   (let [vsym (gensym)]
     `(let [~vsym ~v]
        (and ~@(map (fn [n] `(implements? ~vsym ~n)) (cons name names)))))))

(u/defmac compile-all!
  [] `(do ~@(map forms/protocol-extension (vals (reg/get-reg)))))

(u/defmac type+
  "like extend type"
  [tag & impls]
  `(do ~@(mapv #(forms/implement tag %) impls)))

(u/defmac thing
  "like reify but for generics"
  [& impls]
  (forms/thing impls))

(u/defmac tag+

  "add a type tag to the type registry (living in nemesis.state/state)
   tag: the typetag we are defining/extending (a keyword)
   childs: a seq of other typetags or classes that belongs to the defined tag
   parents: a seq of other typetags that the defined tag belongs to
   & impls: optional generic implementations for the defined tag"

  ([{:keys [tag childs parents impls]}]
   (let [exists? (state/get-in [:types tag])
         generic-updates
         (if exists? (cons tag parents) parents)]

     (state/swap!
       update :types
       forms/conj-type
       {:tag tag
        :childs childs
        :parents parents})

     ;; this is brutal, should only recompute what's nescessary
     (state/swap! assoc :guards (t/predmap))

     #_(p/pp 'not-synced (sync-types-form (vec generic-updates)))

     `(do
        ~(forms/types-syncronisation (vec generic-updates))
        ~(when (seq impls) `(type+ ~tag ~@impls)))))

  ([tag childs]
   `(tag+ ~tag ~childs []))
  ([tag childs parents & impls]
   `(tag+ ~{:tag tag :childs childs :parents parents :impls (vec impls)})))

(u/defmac deft

  "declare a new usertype (a clojure record)
   tag: the typetag (keyword) corresponding to our freshly created record
   fields: the fields of our record
   parents: a seq of other typetags that our type belongs to
   & impls: optional generic implementations for the defined type"

  ([{:as spec
     :keys [tag parents impls fields childs class-sym]}]

   (let [ns-str (str *ns*)
         tag-name (name tag)
         class-str (apply str (map str/capitalize (str/split tag-name #"-")))
         class-sym (or class-sym (symbol class-str))

         qualified-class
         (if (state/cljs?)
           (symbol ns-str (name class-sym))
           (u/sym (str/replace ns-str "-" "_") "." (name class-sym)))

         spec
         (-> (assoc spec :tag (keyword ns-str (name tag)))
             (update :childs (fnil conj []) qualified-class))]

     `(do #_~(unmap-deft-form {})
        (defrecord ~class-sym ~fields)
        (def ~(symbol tag-name) ~(u/sym '-> class-sym))
        (defg ~(u/sym '-> tag-name) [~'_])
        (tag+ ~spec))))

  ([tag fields]
   `(deft ~tag ~fields []))

  ([tag fields x & xs]
   (let [[parents impls] (if (vector? x) [x xs] [[] (cons x xs)])]
     `(deft ~{:tag (keyword tag) :fields fields :parents parents
              :impls (mapv (partial forms/deft_impl-bind-fields fields) impls)}))))
