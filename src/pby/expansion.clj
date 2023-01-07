(ns pby.expansion)

(defonce ^:dynamic *state*
  {:env nil :form nil})

(defmacro expanding [& body]
  `(binding [*state* {:env ~'&env :form ~'&form}]
     ~@body))

(defn host []
  (if (boolean (:ns (:env *state*)))
    :cljs
    :clj))
