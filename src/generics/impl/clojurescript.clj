(ns generics.impl.clojurescript
  (:require [generics.impl.utils :as u]))

(defmacro prototype-assoc [obj meth impl]
  (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))

(defn prototype-assoc-form [obj meth impl]
  (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))

(def base-type
  {nil       "null"
   'object   "object"
   'string   "string"
   'number   "number"
   'array    "array"
   'function "function"
   'boolean  "boolean"
   'default  "_"})

(defn protocol-prefix
  [psym]
  (str (-> (str psym)
           (.replace \. \$)
           (.replace \/ \$))
       "$"))

(defn extend_properties [protocol-name method-name arity]
  (let [psym (symbol protocol-name)
        ns (.replace (namespace psym) \- \_)
        name (name psym)
        prefix (protocol-prefix (symbol ns name))]
    {:sentinel prefix
     :method   (str prefix (munge method-name) "$arity$" arity)}))

(defn extend1 [class protocol method arity impl]
  (if (base-type class)
    (let [class (if (= 'default class) "_" class)]
      `(do (goog.object/set ~protocol ~(str class) true)
           (goog.object/set ~(u/with-ns (namespace protocol) method) ~(str class) ~impl)))
    (let [props (extend_properties protocol method arity)]
      `(do ~(prototype-assoc-form class (:sentinel props) 'cljs.core/PROTOCOL_SENTINEL)
           ~(prototype-assoc-form class (:method props) impl)))))