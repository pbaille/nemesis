(ns thetis.functions.spec
  "Generic function spec manipulation.
   Handles case storage (declaration vs extension), effective-case computation,
   extension mode validation (:sealed/:extend/:refine/:override), cloning, and
   class-level case expansion."
  (:require [thetis.utils.misc :as u]
            [thetis.utils.names :as names]
            [thetis.types.core :as types]))

;; === Effective Cases ===
;;
;; Specs store cases in two places:
;;   :declaration-cases — from defg (the original definition)
;;   :extension-cases   — map of {ns-key [cases...]} from generic+ calls
;;
;; The effective (flat) case list is computed by `effective-cases`:
;;   [...latest-extension-cases, ...earlier-extension-cases, ...declaration-cases]
;;
;; This preserves the precedence semantics: most recently added wins.
;; Extension ordering is by sorted namespace keys (deterministic).
;;
;; `:cases` is kept as a derived field equal to `(effective-cases spec)`
;; for code that reads it directly.

(defn effective-cases
  "Compute the flat case list from structured storage.
   Returns cases in precedence order: latest extensions first, declaration last.
   Spec must have been initialized via init-spec (has :declaration-cases and :extension-cases)."
  [spec]
  (assert (:declaration-cases spec)
          (str "effective-cases requires an initialized spec (missing :declaration-cases). "
               "Call init-spec first. Spec keys: " (keys spec)))
  (let [decl (:declaration-cases spec)
        exts (:extension-cases spec)
        sorted-ext-keys (sort (keys exts))
        ext-cases (mapcat #(get exts %) (reverse sorted-ext-keys))]
    (vec (concat ext-cases decl))))

(defn- guard-parent-types
  "Get the parent types for a type keyword, considering both the type hierarchy
   and the guard registry. A guard's base type is its parent."
  [type-registry guards type-key]
  (let [;; Regular hierarchy parents
        reg-parents (when type-registry (types/parents type-registry type-key))
        ;; If this is a guard type, its base type is a parent
        guard-parents (when-let [gs (and guards (get guards type-key))]
                        [(:base gs)])]
    (set (concat reg-parents guard-parents))))

(defn- check-extend-mode
  "In :extend mode, no existing behavior may be changed at all.
   Blocks both direct overrides (re-implementing :vec when :vec exists)
   and specialization (adding :vec when :coll covers it, or adding :positive when :number covers it).
   This is the safest mode — only truly uncovered types allowed."
  [original-spec extension-spec type-registry & {:keys [guards]}]
  (let [existing (set (map (juxt :type :arity) (effective-cases original-spec)))
        existing-types-by-arity (group-by :arity (effective-cases original-spec))]
    (doseq [{:keys [type arity]} (:cases extension-spec)]
      ;; Direct overlap — type already has its own case
      (assert (not (existing [type arity]))
              (str "Extension mode :extend — cannot override existing implementation for "
                   type " at arity " arity
                   " on generic " (:fullname original-spec)
                   ". Use :refine or :override mode to allow overrides."))
      ;; Specialization check — type is a child of an existing type (via hierarchy or guard base)
      (let [existing-types-at-arity (set (map :type (get existing-types-by-arity arity)))
            parent-types (guard-parent-types type-registry guards type)]
        (assert (not (some existing-types-at-arity parent-types))
                (str "Extension mode :extend — cannot specialize " type " at arity " arity
                     " on generic " (:fullname original-spec)
                     " (already covered by a parent type). "
                     "Use :refine or :override mode to allow specialization."))))))

(defn- check-refine-mode
  "In :refine mode, you can add new types freely and specialize subtypes
   (e.g., add :vec when :coll already has an implementation).
   But you cannot directly override an existing type's implementation."
  [original-spec extension-spec _type-registry]
  (let [existing (set (map (juxt :type :arity) (effective-cases original-spec)))]
    (doseq [{:keys [type arity]} (:cases extension-spec)]
      (assert (not (existing [type arity]))
              (str "Extension mode :refine — cannot override existing implementation for "
                   type " at arity " arity
                   " on generic " (:fullname original-spec)
                   ". " type " already has a direct implementation. "
                   "Use :override mode to allow direct overrides.")))))

(defn merge-cases
  "Merge extension cases into a spec, keyed by namespace for idempotent rebuilds.
   extension-ns is required — identifies which namespace is contributing the cases."
  [original-spec extension-spec extension-ns
   & {:keys [allow-default-overides type-registry guards]}]

  (assert extension-ns "extension-ns is required for merge-cases")

  (assert (every? (-> original-spec :arities keys set)
                  (-> extension-spec :arities keys))
          "unknown arity extension.")

  (assert (or allow-default-overides
              (not (some :default (vals (:arities extension-spec)))))
          "default case can only be defined on generic creation (thetis.core/defg)")

  ;; Mode enforcement
  (case (:mode original-spec)
    :sealed   (throw (ex-info (str "Generic " (:fullname original-spec) " is sealed — no extensions allowed.")
                              {:generic (:fullname original-spec)}))
    :extend   (check-extend-mode original-spec extension-spec type-registry :guards guards)
    :refine   (check-refine-mode original-spec extension-spec type-registry)
    :override nil
    ;; default (nil / unset) — same as :refine
    (check-refine-mode original-spec extension-spec type-registry))

  (let [ns-key (str extension-ns)
        existing (get-in original-spec [:extension-cases ns-key] [])
        updated (assoc-in original-spec [:extension-cases ns-key]
                          (vec (concat existing (:cases extension-spec))))]
    ;; Recompute :cases for backward compatibility
    (assoc updated :cases (effective-cases updated))))

(defn init-spec
  "Initialize a spec with the structured case storage model.
   Called when a spec is first registered via defg.
   Moves :cases into :declaration-cases and sets up :extension-cases."
  [spec]
  (assoc spec
         :declaration-cases (vec (:cases spec))
         :extension-cases {}))

(defn clone
  "Clone a spec under a new name.
   Snapshots the effective cases as the new spec's declaration-cases."
  [{:as original-spec :keys [arities]}
   new-name]
  (let [{:as names :keys [protocol-prefix method-prefix]} (names/name_derive new-name)
        arities (into {} (map (fn [[arity spec]]
                                [arity (merge spec {:protocol-name (names/name_arify protocol-prefix arity)
                                                    :method-name (names/name_arify method-prefix arity)})])
                              arities))
        ;; Snapshot effective cases — the clone gets a frozen copy
        all-cases (effective-cases original-spec)
        cases (mapv (fn [c] (assoc c :cloned true)) all-cases)]
    (merge original-spec
           names
           {:arities arities
            :cases cases
            :declaration-cases cases
            :extension-cases {}
            :cloned-from (:fullname original-spec)
            :mode :override})))

(defn implementers
  "given a generic spec,
       returns a vector of all types that implement the corresponding generic"
  [spec]
  (->> (effective-cases spec)
       (mapcat (fn [{t :type}] (if (set? t) t [t])))
       (into #{})))

(defn class-extensions
  "return a list of cases objects (one for each class)
   taking care of potential overlaps.
   Accepts either a type-registry map (uses thetis.types.core/classes)
   or a class-resolver function (fn [type] -> [classes...]).
   Guard cases (predicate-dispatched) are excluded — they use a separate dispatch path."
  [spec type-registry-or-resolver]
  (let [resolve-classes (if (fn? type-registry-or-resolver)
                          type-registry-or-resolver
                          (fn [t] (types/classes type-registry-or-resolver t)))]
    (letfn [(expand-case [c]
              (map #(assoc c :class %) (resolve-classes (:type c))))
            (conj-case [m {:as _case :keys [class type arity name]}]
              (assoc m [class arity]
                     (merge (-> spec :arities (get arity))
                            {:arity arity
                             :class class
                             :type type
                             :impl-name name})))]
      (->> (reverse (effective-cases spec))
           (remove :guard)  ;; guard cases don't go through protocol dispatch
           (mapcat expand-case)
           (reduce conj-case {})
           (vals)))))
