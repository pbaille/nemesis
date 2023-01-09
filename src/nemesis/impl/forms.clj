(ns nemesis.impl.forms
  (:require [nemesis.types :as t]
            [nemesis.impl.utils :as u]
            [nemesis.impl.registry :as reg]
            [nemesis.state :as state]
            [clojure.core :as c]
            [clojure.set :as set]))

(defn prototype_registering
  [{:as _spec :keys [fullname cloned-from]}
   {:as _case :keys [cloned compiled arity type]}]
  (let [form (fn [type]
               `(swap! nemesis.core/prototypes
                       assoc-in
                       [~type '~fullname ~arity]
                       ~(if cloned
                          `(get-in @nemesis.core/prototypes [~type '~cloned-from ~arity])
                          `(fn ~compiled))))]
    (if (set? type)
      `(do ~@(mapv form type))
      (form type))))

(defn prototypes_registering [spec]
  (->> (:cases spec)
       (mapv (partial prototype_registering spec))
       (list* 'do)))

(do :cljs-extend

    (let [cljs-base-type
          {nil       "null"
           'object   "object"
           'string   "string"
           'number   "number"
           'array    "array"
           'function "function"
           'boolean  "boolean"
           'default  "_"}

          protocol-prefix
          (fn [psym]
            (str (-> (str psym)
                     (.replace \. \$)
                     (.replace \/ \$))
                 "$"))

          cljs-extend_properties
          (fn [protocol-name method-name arity]
            (let [psym (symbol protocol-name)
                  ns (.replace (namespace psym) \- \_)
                  name (name psym)
                  prefix (protocol-prefix (symbol ns name))]
              {:sentinel prefix
               :method   (str prefix (munge method-name) "$arity$" arity)}))]

      (defn cljs-extend1 [class protocol method arity impl]
        (if-let [class-str (cljs-base-type class)]
          `(do (goog.object/set ~protocol ~class-str true)
               (goog.object/set ~(u/with-ns (namespace protocol) method) ~class-str ~impl))
          (let [props (cljs-extend_properties protocol method arity)]
            `(do ~(u/cljs_prototype-assoc-form class (:sentinel props) 'cljs.core/PROTOCOL_SENTINEL)
                 ~(u/cljs_prototype-assoc-form class (:method props) impl)))))))

(defn extension-form
  [{:as _spec :keys [ns fullname]}
   {:as _case :keys [class arity type protocol-name method-name]}]
  (let [type (if (set? type) (first type) type)
        impl `(get-in @nemesis.core/prototypes [~type '~fullname ~arity])]

    (if (state/cljs?)

      (cljs-extend1 class (u/with-ns ns protocol-name)
                    method-name arity impl)
      (list 'do
            (when class (list `c/import (list 'quote class)))
            (list `c/extend class
                  (u/with-ns ns protocol-name)
                  {(keyword method-name) impl})))))

(defn protocol-declaration
  [{:keys [arities ns]}]
  `(do ~@(mapv (fn [[_ {:keys [protocol-name method-name argv]}]]
                 `(defprotocol ~(u/with-ns ns protocol-name)
                    ~(list method-name argv)))
               arities)))

(defn protocol-extension
  [spec]
  `(do ~@(mapv (partial extension-form spec)
               (reg/class-extensions spec))))

(defn function-definition
  [{:keys [name arities]}]
  `(defn ~name
     ~@(mapv (fn [{:keys [variadic argv method-name]}]
               (list
                (if variadic
                  (u/argv_variadify argv)
                  argv)
                `(~method-name ~@argv)))
             (vals arities))))

(defn extension [spec]
  `(do
     ~(prototypes_registering spec)
     ~(protocol-extension spec)))

(defn cleaning [{:keys [ns name arities]}]
  `(do
     ~@(mapv (fn [x#] `(ns-unmap '~(symbol ns) '~x#))
             (cons name (mapcat (juxt :method-name :protocol-name) (vals arities))))))

(defn declaration [spec]
  #_(println p/*cljs*)
  `(do ~(cleaning spec)
       ~(protocol-declaration spec)
       ~(function-definition spec)
       ~(prototypes_registering spec)
       ~(protocol-extension spec)
       ~(:name spec)))

(do :sync

    (defn implementers
      "given a generic spec,
       returns a vector of all types that implement the corresponding generic"
      [spec]
      (->> (:cases spec)
           (mapcat (fn [{t :type}] (if (set? t) t [t])))
           (into #{})))


    (defn implementers-map []
      (u/$vals (reg/get-reg) implementers))

    (defn types-syncronisation
      "when type registry has been updated,
       we sometimes need to sync some generics declaration
       xs: the types that have changed
       only the generics impacted by this change will be synced"
      [xs]
      (let [sync? #(seq (set/intersection (set xs) (set %)))]
        (cons 'do
              (mapv (fn [[name ts]]
                      #_(println "sync-types-form " (sync? ts) (pr-str name))
                      ;; TODO we could emit only whats needed instead of all impls...
                      (when (sync? ts) (protocol-extension (reg/get-spec! name))))
                    (implementers-map))))))

(do :implement


    (defn implement_impl-body->cases
      [tag cases]
      (map (fn [[pat bod]] (list pat tag bod))
           (u/fn-cases_normalize cases)))


    (defn implement [tag [name & body :as form]]

      #_(println "implementing " tag (state/qualify-symbol name) #_form #_(keys (get-reg)))
      (if (reg/get-spec name)
        `(nemesis.core/generic+ ~name ~@(implement_impl-body->cases tag body))
        ;; TODO do I want to restore that ? it does not works well with refreshing
        (if-let [p (-> (resolve name) meta :protocol)]
          `(extend-protocol ~(symbol p)
             ~@(doall (mapcat (fn [t] [t form]) (t/classes tag))))))))

(do :thing

    (defn thing_parse-impl-cases
      [[name & cases]]
      (let [{:as _spec :keys [ns method-prefix protocol-prefix]}
            (reg/get-spec! name)
            with-clean-pattern
            (fn [x] (update x :pattern (comp vec (partial remove #{'&}))))
            with-variadic-flag
            (fn [x] (assoc x :variadic (u/argv_variadic? (:pattern x))))
            qualify
            #(symbol (c/name ns) (c/name %))
            with-names
            (fn [x]
              (let [ari (count (:pattern x))]
                (assoc x :arity ari
                         :method-name (qualify (u/name_arify method-prefix ari))
                         :protocol-name (qualify (u/name_arify protocol-prefix ari)))))]

        (->> (u/fn-cases_normalize cases)
             (map (fn [[pat bod]] {:pattern pat :body bod}))
             (map with-variadic-flag)
             (map with-clean-pattern)
             (map with-names))))

    (defn thing_cases->decls [xs]
      (mapcat (fn [{:keys [method-name protocol-name body pattern]}]
                [protocol-name (list method-name pattern body)])
              xs))

    (defn thing [impls]
      `(reify
         ~@(mapcat thing_cases->decls
                   (map thing_parse-impl-cases impls)))))

(do :fork
    (defn fork [new-name original-name]

      `(swap! nemesis.core/prototypes
              assoc ~new-name (get @nemesis.core/prototypes ~original-name))))

(do :type-extension

    (defn conj-type [reg {:keys [tag childs parents]}]
      (reduce
       (fn [reg p]
         (update reg p (fnil conj #{}) tag))
       (update reg tag (fnil into #{}) childs)
       parents))

    (defn deft_impl-bind-fields [fields [name argv & body]]
      (let [[this-sym this-pat]
            (u/binding-pattern_ensure-top-level-sym (first argv) (gensym))]
        `(~name ~(vec (cons this-pat (next argv)))
          (let [{:keys ~fields} ~this-sym] ~@body))))
    )

(do :inheritance

    (defn prototypes_learn [protos type parents]
      (assoc protos
        type
        (reduce (fn [proto parent]
                  (merge proto (get protos parent)))
                (get protos type)
                parents)))

    (defn prototypes_teach [protos type childs]
      (let [proto (get protos type)]
        (reduce (fn [protos child]
                  (update protos child merge proto))
                protos
                childs)))

    (defn prototypes_diff [ps1 ps2]
      (reduce (fn [ps [type p2]]
                (update ps type u/map_diff p2))
              ps1 ps2)))
