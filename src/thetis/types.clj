(ns thetis.types
  "User-facing type operations for thetis.
   Provides hierarchy queries, type predicates, and registry access.
   Reads from thetis.state — part of the impure shell."
  (:refer-clojure :exclude [parents >= <=])
  (:require [thetis.state :as state]
            [thetis.types.core :as types]
            [thetis.types.data :as data]
            [clojure.core :as c]))

;; === Type data — re-exported from thetis.types.data ===

(def clj-base-types data/clj-base-types)
(def cljs-base-types data/cljs-base-types)
(def groups data/groups)
(def preds-symbols data/preds-symbols)

;; === Registry access ===

(defn get-reg []
  (state/get :types))

(defn get-type [t]
  (get (get-reg) t))

;; === Hierarchy queries ===

(defn all-paths
  ([] (types/all-paths (get-reg)))
  ([x] (types/all-paths x))
  ([x lines] (types/all-paths x lines)))

(defn cyclic? [x] (types/cyclic? x))

(defn childs
  "Children seq."
  ([x] (types/children (get-reg) x))
  ([reg x] (types/children reg x)))

(defn parents
  "Parents seq."
  ([x] (types/parents (get-reg) x))
  ([reg x] (types/parents reg x)))

(defn childof
  "Is x a child of y?"
  ([x y] (types/childof (get-reg) x y))
  ([reg x y] (types/childof reg x y)))

(defn parentof
  "Is x a parent of y?"
  ([x y] (types/parentof (get-reg) x y))
  ([reg x y] (types/parentof reg x y)))

(defn classes
  "Returns all classes for a type.
   Resolves type keywords to concrete classes via the registry."
  ([t] (types/classes (get-reg) t))
  ([reg t] (types/classes reg t)))

(defn all-types
  "Return a set containing all types of a registry."
  ([] (all-types (get-reg)))
  ([reg] (into #{} (keys reg))))

;; === Predicate compilation ===

(defn symbolic-preds->or-form [ps]
  (if (= (count ps) 1)
    (first ps)
    `(c/or ~@ps)))

(defn symbolic-pred-body [reg k gsym]
  (if-let [psym (preds-symbols k)]
    (list psym gsym)
    (if (symbol? k)
      (list `instance? k gsym)
      (when-let [members (reg k)]
        (symbolic-preds->or-form
         (map #(symbolic-pred-body reg % gsym) members))))))

(defn symbolic-pred
  ([k] (symbolic-pred (get-reg) k))
  ([reg k]
   (if-not (map? reg)
     (symbolic-pred (get-reg) reg k)
     (do
       (assert (reg k) (str [:unknown-type k]))
       (let [gsym (gensym)]
         `(fn [~gsym] (when ~(symbolic-pred-body reg k gsym) ~gsym))))))
  ([reg k seed]
   (assert (reg k) (str [:unknown-type k]))
   (let [gsym (gensym)]
     `(let [~gsym ~seed]
        (when ~(symbolic-pred-body reg k gsym) ~gsym)))))

(defmacro isa
  ([t] `(fn [x#] (isa ~t x#)))
  ([t x]
   (cond (get-type t) `(~(symbolic-pred t) ~x)
         (set? t) `(or ~@(map (fn [t] (macroexpand (list `isa t x))) t))
         :else (throw (ex-info (str "thetis.types/isa - Unknown type: " t) {:type t})))))
