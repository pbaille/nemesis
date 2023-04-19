(ns poly.compiler.data)

(def initial
  (let [base {:functions {}
              :types {}
              :guards {}
              :namespaces {}}]
    {:clj base
     :cljs base}))
