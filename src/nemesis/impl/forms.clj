(ns nemesis.impl.forms
  (:require [nemesis.types :as t]
            [nemesis.impl.utils :as u]
            [nemesis.impl.registry :as reg]
            [nemesis.state :as state]
            [clojure.core :as c]
            [clojure.set :as set]))

(defn prototype_registering
  [{:as _spec :keys [fullname]}
   {:as _case :keys [compiled arity type]}]
  (let [form (fn [type]
               `(swap! nemesis.core/prototypes
                       assoc-in
                       [~type '~fullname ~arity]
                       (fn ~compiled)))]
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
        (if (cljs-base-type class)
          (let [class (if (= 'default class) "_" class)]
            `(do (goog.object/set ~protocol ~(str class) true)
                 (goog.object/set ~(u/with-ns (namespace protocol) method) ~(str class) ~impl)))
          (let [props (cljs-extend_properties protocol method arity)]
            `(do ~(u/cljs_prototype-assoc-form class (:sentinel props) 'cljs.core/PROTOCOL_SENTINEL)
                 ~(u/cljs_prototype-assoc-form class (:method props) impl)))))))

(defn extend-forms
  [{:as spec :keys [ns fullname]}]

  (mapv
   (fn [[[class arity]
         {:keys [type protocol-name method-name]}]]

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

   (reg/extension-map spec)))


(defn protocol-declaration
  [{:keys [arities ns]}]
  `(do ~@(mapv (fn [[_ {:keys [protocol-name method-name argv]}]]
                 `(defprotocol ~(u/with-ns ns protocol-name)
                    ~(list method-name argv)))
               arities)))


(defn protocol-extension
  [spec]
  `(do ~@(extend-forms spec)))


(defn function-definition_initial
  [{:keys [name arities variadic]}]
  (let [arities (sort arities)
        fixed-arities (if variadic (butlast arities) arities)]
    `(defn ~name
       ~@(mapv (fn [{:keys [argv method-name protocol-name default]}]
                 `(~argv (if (satisfies? ~protocol-name ~(first argv))
                           (~method-name ~@argv)
                           ~default)))
               (vals fixed-arities))
       ~@(when variadic
           (let [variadic-arity (val (last arities))
                 vsig (:argv variadic-arity)]
             [`(~(u/argv_variadify vsig)
                (~(:method-name variadic-arity) ~@vsig))])))))

(defn function-definition
  [{:keys [name arities]}]
  `(defn ~name
     ~@(mapv (fn [{:keys [variadic argv method-name protocol-name default]}]
               (list
                (if variadic
                  (u/argv_variadify argv)
                  argv)
                `(if (satisfies? ~protocol-name ~(first argv))
                   (~method-name ~@argv)
                   (let ~(vec (interleave (:argv default) argv))
                     ~(:expr default)))))
             (vals arities))))


(defn extension [spec]
  #_(pp "extform" (get-spec! (:name spec)))
  (let [original-spec (reg/get-spec! (:fullname spec))
        extended-spec (reg/extend-spec original-spec spec)
        ;; some cases were potentially eliminated by reg/conj-case, we are removing them
        partial-spec (update spec :cases (partial filter (set (:cases extended-spec))))]
    (reg/register-spec! extended-spec)
    `(do ;; ~(dispatches-declarations partial-spec)
       ~(prototypes_registering partial-spec)
       ~(protocol-extension partial-spec))))

(defn cleaning [{:keys [ns name arities]}]
  `(do
     ~@(mapv (fn [x#] `(ns-unmap '~(symbol ns) '~x#))
             (cons name (mapcat (juxt :method-name :protocol-name) (vals arities))))))


(defn declaration [spec]
  #_(println p/*cljs*)
  (reg/register-spec! spec)
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
        (do ;p/prob 'sync-form
          (cons 'do
                (mapv (fn [[name ts]]
                        #_(println "sync-types-form " (sync? ts) (pr-str name))
                        (when (sync? ts) (protocol-extension (reg/get-spec! name))))
                      (implementers-map)))))))

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
