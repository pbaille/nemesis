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


