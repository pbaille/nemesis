(ns poly.compiler.forms
  (:require [poly.utils.misc :as u]
            [poly.utils.expansion :as expansion]
            [poly.compiler.views :as compiler]
            [clojure.core :as c]
            [clojure.set :as set]))

(do :prototypes

    (defn prototype_registering
      [{:as _spec :keys [fullname cloned-from]}
       {:as _case :keys [cloned compiled arity type]}]
      (let [form (fn [type]
                   `(swap! poly.core/prototypes
                           assoc-in
                           [~type '~fullname ~arity]
                           ~(if cloned
                              `(get-in @poly.core/prototypes [~type '~cloned-from ~arity])
                              `(fn ~compiled))))]
        (if (set? type)
          `(do ~@(mapv form type))
          (form type))))

    (defn prototypes_registering [spec]
      (->> (:cases spec)
           (mapv (partial prototype_registering spec))
           (list* 'do))))

(do :extension

    (do :cljs-extend

        (let [cljs-base-type
              {nil       "null"
               'object   "object"
               'string   "string"
               'number   "number"
               'array    "array"
               'function "function"
               'boolean  "boolean"
               'default  "_"}]

          (letfn [(cljs_prototype-assoc-form [obj meth impl]
                    (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))

                  (protocol-prefix [psym]
                    (str (-> (str psym)
                             (.replace \. \$)
                             (.replace \/ \$))
                         "$"))

                  (cljs-extend_properties
                    [protocol-name method-name arity]
                    (let [psym (symbol protocol-name)
                          ns (.replace (namespace psym) \- \_)
                          name (name psym)
                          prefix (protocol-prefix (symbol ns name))]
                      {:sentinel prefix
                       :method   (str prefix (munge method-name) "$arity$" arity)}))]

            (defn cljs-extend1 [class protocol method arity impl]
              (if-let [class-str (cljs-base-type class)]
                `(do (cljs.core/unchecked-set ~protocol ~class-str true)
                     (cljs.core/unchecked-set ~(u/with-ns (namespace protocol) method) ~class-str ~impl))
                (let [props (cljs-extend_properties protocol method arity)]
                  `(do ~(u/cljs_prototype-assoc-form class (:sentinel props) 'cljs.core/PROTOCOL_SENTINEL)
                       ~(u/cljs_prototype-assoc-form class (:method props) impl))))))))

    (defn extension-form
      [{:as _compiler :keys [expansion]}
       {:as _spec :keys [ns fullname]}
       {:as _case :keys [class arity type protocol-name method-name]}]
      (let [type (if (set? type) (first type) type)
            impl `(get-in @poly.core/prototypes [~type '~fullname ~arity])
            qualified-protocol (u/with-ns ns protocol-name)]

        (if (expansion/cljs? expansion)

          (cljs-extend1 class qualified-protocol
                        method-name arity impl)
          (list 'do
                (when class (list `c/import (list 'quote class)))
                (list `c/extend class
                      qualified-protocol
                      {(keyword method-name) impl})))))

    (defn extend-class [compiler class]
      `(do ~@(reduce (fn [ret {:keys [spec cases]}]
                       (into ret (mapv (partial extension-form compiler spec)
                                       cases)))
                     [] (compiler/get-class-cases compiler class))))

    (defn protocol-extension
      [compiler spec]
      `(do ~@(mapv (partial extension-form compiler spec)
                   (compiler/class-extensions compiler spec))))

    (defn extension [compiler spec]
      `(do
         ~(prototypes_registering spec)
         ~(protocol-extension compiler spec))))

(do :declaration

    (defn protocol-declaration
      [{:keys [arities ns]}]
      `(do ~@(mapv (fn [[_ {:keys [protocol-name method-name argv]}]]
                     `(defprotocol ~(u/with-ns ns protocol-name)
                        ~(list method-name argv)))
                   arities)))

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

    (defn cleaning [{:keys [ns name arities]}]
      `(do
         ~@(mapv (fn [x#] `(ns-unmap '~(symbol ns) '~x#))
                 (cons name (mapcat (juxt :method-name :protocol-name) (vals arities))))))

    (defn declaration [compiler spec]
      #_(println p/*cljs*)
      `(do ~(cleaning spec)
           ~(protocol-declaration spec)
           ~(function-definition spec)
           ~(prototypes_registering spec)
           ~(protocol-extension compiler spec)
           ~(:name spec))))

(do :implement


    (defn implements-all? [expr specs]
      (if (symbol? expr)
        `(and ~@(mapv (fn [spec]
                        `(when (or ~@(mapv (fn [protocol-name] `(satisfies? ~(u/with-ns (:ns spec) protocol-name) ~expr))
                                           (map (comp :protocol-name val) (:arities spec))))
                           ~expr))
                      specs))
        (let [vsym (gensym)]
          `(let [~vsym ~expr]
             ~(implements-all? vsym specs)))))

    (defn implement_impl-body->cases
      [tag cases]
      (mapv (fn [[pat bod]] (list pat tag bod))
            (u/fn-cases_normalize cases)))

    (defn implement [tag [name & body]]
      `(nemesis.core/generic+
        ~name
        ~@(implement_impl-body->cases tag body))))

(do :thing

    (defn thing_parse-impl-cases
      [compiler [name & cases]]
      (let [{:as _spec :keys [ns method-prefix protocol-prefix]}
            (compiler/get-function! compiler name)
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

    (defn thing [compiler impls]
      `(reify
         ~@(mapcat thing_cases->decls
                   (map (partial thing_parse-impl-cases compiler) impls)))))

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
