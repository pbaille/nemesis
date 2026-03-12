(ns thetis.utils.expansion
  "Expansion context utilities.
   Determines compilation target (CLJ vs CLJS) and resolves symbols
   using the macro expansion environment."
  (:require cljs.analyzer))

(defmacro state []
  '{:env &env :form &form})

(defn cljs?
  "Is the current expansion targeting ClojureScript?
   Detected by the presence of :ns in the &env map."
  [state]
  (boolean (get-in state [:env :ns])))

(defn qualify-symbol
  "Resolve a symbol to its fully-qualified form using the expansion environment.
   Uses `cljs.analyzer/resolve-var` for CLJS, `resolve` for CLJ."
  [state x]
  (when (symbol? x)
    (if (cljs? state)
      (:name (cljs.analyzer/resolve-var (:env state) x))
      (if-let [v (resolve (:env state) x)]
        (let [{:keys [ns name]} (meta v)]
          (symbol (str ns) (str name)))))))
