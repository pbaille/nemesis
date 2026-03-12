(ns thetis.compiler.core
  "Pure compiler operations: queries and updates on the compiler value.
   The compiler is a map with :functions (spec registry), :types (type registry),
   :guards, :type-state, :guard-state, and :expansion context."
  (:require [thetis.functions.registry :as fn.reg]
            [thetis.functions.spec :as fn.spec]
            [thetis.types.core :as types]))

;; === Queries ===

(defn get-function
  "Look up a function spec by name, returns nil if not found."
  [compiler name]
  (get (:functions compiler) name))

(defn get-function!
  "Look up a function spec by name, throws if not found."
  [compiler name]
  (fn.reg/get! (:functions compiler) name))

(defn get-class-cases [compiler class]
  (fn.reg/get-class-cases (:functions compiler) (:types compiler) class))

(defn type->classes [compiler type]
  (types/classes (:types compiler) type))

(defn class-extensions
  "Return a list of case objects (one for each class)
   taking care of potential overlaps.
   This can be used to emit `extend-type` or `extend` forms."
  [compiler spec]
  (fn.spec/class-extensions spec (:types compiler)))

(defn implementers-map
  "Map of generic-name → set of implementing types."
  [compiler]
  (fn.reg/implementers-map (:functions compiler)))

;; === Updates (compiler, args) → compiler' ===

(defn add-function
  "Register a new function spec in the compiler."
  [compiler spec]
  (update compiler :functions fn.reg/add-spec spec))

(defn extend-function
  "Extend an existing function with new cases, keyed by namespace for idempotent rebuilds.
   extension-ns is required."
  [compiler extension-spec extension-ns]
  (update compiler :functions fn.reg/extend-spec extension-spec extension-ns
          :type-registry (:types compiler)
          :guards (:guards compiler)))

(defn clone-function
  "Clone a function spec under a new name."
  [compiler original-name new-name]
  (update compiler :functions fn.reg/clone-spec original-name new-name))

(defn add-type
  "Add a new type to the type registry.
   `members` must all be existing keys in the registry."
  [compiler tag members]
  (update compiler :types types/add-type tag members))

(defn remove-type
  "Remove a type from the type registry."
  [compiler tag]
  (update compiler :types types/remove-type tag))

(defn update-types
  "Apply an arbitrary function to the type registry.
   For cases where add-type/remove-type aren't sufficient
   (e.g. register-type with parent groups)."
  [compiler f & args]
  (apply update compiler :types f args))

(defn remove-ns-contributions
  "Remove all contributions from a given namespace.
   Removes function specs/extensions defined in that ns.
   Also removes type and guard contributions."
  [compiler ns-str]
  (let [c (update compiler :functions fn.reg/remove-ns-contributions ns-str)
        c (if (:type-state c)
            (let [ts (types/remove-type-contributions (:type-state c) ns-str)]
              (assoc c :type-state ts :types (types/effective-types ts)))
            c)
        c (if (:guard-state c)
            (let [gs (types/remove-guard-contributions (:guard-state c) ns-str)]
              (assoc c :guard-state gs :guards (types/effective-guards gs)))
            c)]
    c))

(defn register-type-contribution
  "Register a type contribution from a namespace.
   Updates the structured :type-state and recomputes the flat :types view atomically."
  [compiler ns-str tag members groups]
  (let [ts (types/register-type-contribution (:type-state compiler) ns-str tag members groups)]
    (assoc compiler :type-state ts :types (types/effective-types ts))))

;; === Guard operations ===

(defn get-guard
  "Look up a guard spec by keyword."
  [compiler guard-key]
  (get (:guards compiler) guard-key))

(defn guard?
  "Is this type keyword a guard (predicate type)?"
  [compiler type-key]
  (boolean (get-guard compiler type-key)))

(defn register-guard
  "Register a predicate guard type.
   Updates guard-state and guards map.
   Does NOT register in the type registry — guards are not class-based types.
   Extension mode checks use the guard registry directly for hierarchy awareness."
  [compiler ns-str guard-key {:keys [base pred] :as guard-spec}]
  (assert (keyword? guard-key) (str "guard key must be a keyword: " guard-key))
  (assert (keyword? base) (str "guard :base must be a type keyword: " base))
  (assert ((:types compiler) base) (str "guard :base must be a registered type: " base))
  (let [gs (types/register-guard-contribution
            (:guard-state compiler) ns-str guard-key guard-spec)]
    (assoc compiler
           :guard-state gs
           :guards (types/effective-guards gs))))

(defn remove-guard-contributions
  "Remove all guard contributions from a namespace."
  [compiler ns-str]
  (if (:guard-state compiler)
    (let [gs (types/remove-guard-contributions (:guard-state compiler) ns-str)]
      (assoc compiler :guard-state gs :guards (types/effective-guards gs)))
    compiler))
