(ns thetis.types.core
  "Pure type registry operations.
   The registry is a map of {type-keyword → #{members...}} where members
   are either concrete class symbols or other type keywords (for groups).
   Provides hierarchy queries: children, parents, classes, isa, paths."
  (:refer-clojure :exclude [parents])
  (:require [clojure.set :as set]
            [thetis.utils.misc :as u]))

(defn add-type
  "Register a type keyword with its member classes/keywords.
   Keyword members must already exist in the registry."
  [reg tag members]
  (assert (keyword? tag)
          (str "type tag must be a keyword: " tag))
  (let [kw-members (filter keyword? members)]
    (assert (every? reg kw-members)
            (str "unknown keyword members for type tag " tag ": "
                 (remove reg kw-members))))
  (assoc reg tag (set members)))

(defn remove-type
  "Remove a type keyword and all references to it from the registry."
  [reg t]
  (->> (dissoc reg t)
       (map (fn [[k v]] [k (disj v t)]))
       (into {})))

(defn all-paths
  "Enumerate all paths through the type hierarchy.
   Detects cycles and throws if found."
  ([x]
   (all-paths x (map vector (keys x))))
  ([x lines]
   (doall
    (mapcat
     (fn [l]
       (let [ll (last l)
             xs (seq (get x ll))
             overlap (seq (set/intersection (set l) (set xs)))]
         (cond (not xs) [l]
               overlap (u/error "cycle! " l)
               :else (all-paths x (map (partial conj l) xs)))))
     lines))))

(defn cyclic?
  "Returns true if the type registry contains a cycle."
  [x]
  (try (not (all-paths x))
       (catch Exception _ true)))

(defn children
  "children seq"
  [reg k]
  (if (set? k)
    (mapcat (partial children reg) k)
    (loop [ret [] ps (all-paths reg [[k]])]
      (if-let [ps (seq (filter seq ps))]
        (let [new (set/difference (set (map first ps)) (set ret))]
          (recur (concat ret new)
                 (map rest ps)))
        (vec (rest ret))))))

(defn parents
  "parents seq"
  [reg x]
  (when-let [ps (keys (filter (fn [[_ xs]] (xs x)) reg))]
    (concat ps (mapcat (partial parents reg) ps))))

(defn childof
  "is x a child of y?"
  [reg x y]
  (when (some #{x} (children reg y))
    x))

(defn parentof
  "is x a parent of y?"
  [reg x y]
  (when (some #{y} (children reg x))
    x))

(defn classes
  "Resolve a type keyword to its concrete classes.
   Recursively expands keyword members via the hierarchy.
   Returns nil for unknown keywords, [t] for symbols and nil."
  [reg t]

  (cond

    (or (symbol? t)
        (nil? t)) [t]

    (set? t)
    (mapcat #(classes reg %) t)

    (keyword? t)
    (when-let [cs (reg t)]
      (->> cs
           (mapcat #(children reg %))
           (concat cs)
           (remove keyword?)))))

;; === Namespace-keyed type contributions ===
;;
;; Type state is split into base (from init!) and contributions (from register-type per namespace).
;; This mirrors the function registry's namespace-keyed model for incremental builds.
;;
;; Shape of type-state:
;;   {:base    {:vec #{IPV} :coll #{:seq :vec :set :map} ...}     ;; from init!
;;    :contributions {"ns.a" {:tags {:my-point #{MyPoint}}         ;; tags this ns defined
;;                            :groups {:map #{:my-point}}}}        ;; group memberships this ns added
;;
;; effective-types computes the flat registry by merging base + all contributions.

(defn make-type-state
  "Create a type state from a base registry."
  [base-types]
  {:base base-types
   :contributions {}})

(defn effective-types
  "Compute the flat type registry from base + all contributions."
  [type-state]
  (reduce-kv
   (fn [reg _ns-key {:keys [tags groups]}]
     (let [reg (reduce-kv (fn [r tag members] (assoc r tag members)) reg tags)]
       (reduce-kv (fn [r group new-members]
                    (update r group (fnil into #{}) new-members))
                  reg groups)))
   (:base type-state)
   (:contributions type-state)))

(defn register-type-contribution
  "Register a type contribution from a namespace.
   Accumulates under the namespace key (within the same build pass)."
  [type-state ns-str tag members groups]
  (let [ns-key (str ns-str)
        existing (get-in type-state [:contributions ns-key] {:tags {} :groups {}})
        updated-tags (assoc (:tags existing) tag (set members))
        updated-groups (reduce (fn [gs group]
                                 (update gs group (fnil conj #{}) tag))
                               (:groups existing) groups)]
    (assoc-in type-state [:contributions ns-key]
              {:tags updated-tags :groups updated-groups})))

(defn remove-type-contributions
  "Remove all type contributions from a given namespace."
  [type-state ns-str]
  (update type-state :contributions dissoc (str ns-str)))

;; === Guard (predicate type) support ===

(defn effective-guards
  "Compute the flat guard map from base + all contributions."
  [guard-state]
  (reduce-kv
   (fn [guards _ns-key ns-guards]
     (merge guards ns-guards))
   (:base guard-state)
   (:contributions guard-state)))

(defn register-guard-contribution
  "Register a guard contribution from a namespace."
  [guard-state ns-str guard-key guard-spec]
  (assoc-in guard-state [:contributions (str ns-str) guard-key] guard-spec))

(defn remove-guard-contributions
  "Remove all guard contributions from a given namespace."
  [guard-state ns-str]
  (update guard-state :contributions dissoc (str ns-str)))
