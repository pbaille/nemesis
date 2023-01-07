(ns generics.impl.forms
  (:require [generics.impl.utils :as u]
            [generics.impl.clojurescript :as cljs]
            [clojure.core :as c]))

(def PROTOTYPES_VAR_SYMBOL
  'generics.core/prototypes)

(defn no-implementation-error-form [name argv]
  (let [syms (disj (set (u/findeep argv symbol?)) '&)
        quot (partial list 'quote)]
    (u/error-form "missing implementation for generic: " (quot name)
                  "\n  pattern:\n  " (quot argv)
                  "\n  where:\n  " (zipmap (map quot syms)
                                           syms))))

(defn prototype_registering
  [{:as spec :keys [fullname arities]}
   {:as case :keys [argv expr arity type ns]}]
  (let [form (fn [type]
               `(swap! ~PROTOTYPES_VAR_SYMBOL
                       assoc-in
                       [~type '~fullname ~arity]
                       (fn ~argv ~expr)))]
    (if (set? type)
      `(do ~@(mapv form type))
      (form type))))

(defn prototypes_registering [spec]
  (->> (:cases spec)
       (mapv (partial prototype_registering spec))
       (list* 'do)))

(defn extend-forms
  [{:as spec :keys [ns fullname]}]

  (mapv
   (fn [[[class arity]
         {:keys [type protocol-name method-name]}]]

     (let [type (if (set? type) (first type) type)
           impl `(get-in (deref ~PROTOTYPES_VAR_SYMBOL) [~type '~fullname ~arity])]

       (if (state/cljs?)

         (cljs/extend1 class (u/with-ns ns protocol-name)
                       method-name arity impl)
         (list 'do
               (when class (list `c/import (list 'quote class)))
               (list `c/extend class
                     (u/with-ns ns protocol-name)
                     {(keyword method-name) impl})))))

   (reg/extension-map spec)))