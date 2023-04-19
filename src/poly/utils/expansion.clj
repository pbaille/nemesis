(ns poly.utils.expansion
  (:require cljs.analyzer))

(defmacro state []
  '{:env &env :form &form})

(defn cljs? [state]
  (boolean (get-in state [:env :ns])))

(defn qualify-symbol [state x]
  (when (symbol? x)
    (if (cljs? state)
      (:name (cljs.analyzer/resolve-var (:env state) x))
      (if-let [v (resolve (:env state) x)]
        (let [{:keys [ns name]} (meta v)]
          (symbol (str ns) (str name)))))))
