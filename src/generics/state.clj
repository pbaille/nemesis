(ns generics.state
  (:refer-clojure :exclude [reset! swap! get get-in])
  (:require [clojure.core :as c]
            cljs.analyzer))

(def state0
  (let [base {:fns {}
              :types {}
              :guards {}
              :namespaces {}
              :prototypes {}
              :expansion {}}]
    {:clj base
     :cljs base}))

(defonce state (atom state0))

(defonce debug (atom nil))

(defn env [] (:env (:expansion @state)))
(defn form [] (:form *expansion-state*))
(defn cljs? [] (boolean (:ns (env))))
(defn clj-state [] (:clj @state))
(defn cljs-state [] (:cljs @state))

(defn current []
  (if (cljs?) (cljs-state) (clj-state)))

(defn get
  ([] (current))
  ([k] (c/get (current) k)))

(defn get-in [p]
  (c/get-in (current) p))

(defn compilation-target []
  (if (cljs?) :cljs :clj))

(defmacro expanding [& body]
  `(binding [*expansion-state* {:env ~'&env :form ~'&form}]
     ~@body))

(defmacro targeting-cljs [& xs]
  `(binding [*expansion-state* {:env {:ns true}}]
     ~@xs))

(defn swap! [f & args]
  (c/swap! state update (compilation-target) #(apply f % args)))

(defn reset!
  ([] (reset! state0))
  ([x] (clojure.core/reset! state x)))

(defn qualify-symbol [x]
  (when (symbol? x)
    (if (cljs?)
      (:name (cljs.analyzer/resolve-var (env) x))
      (if-let [v (resolve (env) x)]
        (let [{:keys [ns name]} (meta v)]
          (symbol (str ns) (str name)))))))

(defmacro display []
  (list 'quote @state))