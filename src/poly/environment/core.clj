(ns poly.compiler.core
  (:require [poly.functions.registry :as fn.reg]
            [poly.functions.spec :as fn.spec]
            [poly.types.core :as types]))

(defn get-function! [compiler name]
  (fn.reg/get! (:functions compiler) name))

(defn get-class-cases [compiler class]
  (fn.reg/get-class-cases (:functions compiler) class))

(defn type->classes [compiler type]
  (types/classes (:types compiler) type))

(defn class-extensions
  "return a list cases objects (one for each class)
   taking case of potential overlaps.
   this can be used to emit 'extend-type or 'extend forms"
  [compiler spec]
  (fn.spec/class-extensions spec (:types compiler)))
