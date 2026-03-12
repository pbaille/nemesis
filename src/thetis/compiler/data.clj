(ns thetis.compiler.data
  "Compiler data constructors and platform-initialized compiler values."
  (:require [thetis.types.data :as type-data]))

(defn make-compiler
  "Create a fresh compiler value.
   Optionally takes a type registry; defaults to empty."
  ([]
   (make-compiler {}))
  ([types]
   {:functions {}
    :types types}))

(def clj-compiler
  "A compiler initialized with Clojure base types and groups."
  (make-compiler (merge type-data/clj-base-types type-data/groups)))

(def cljs-compiler
  "A compiler initialized with ClojureScript base types and groups."
  (make-compiler (merge type-data/cljs-base-types type-data/groups)))
