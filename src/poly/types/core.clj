(ns poly.types.core
  (:require [clojure.set :as set]
            [poly.utils :as u]))

(defn add-type [reg tag members]
  (assert (every? reg members)
          (str "unknown members for type tag: " tag))
  (assert (keyword? tag)
          (str "type tag must be a keyword: " tag))
  (assoc reg tag (set members)))

(defn remove-type [reg t]
  (->> (dissoc reg t)
       (map (fn [[k v]] [k (disj v t)]))
       (into {})))

(defn all-paths
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

(defn cyclic? [x]
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
  (when (set/subset? (set (children reg x)) (set (children reg y)))
    x))

(defn parentof
  "is x a parent of y?"
  [reg x y]
  (when (set/subset? (set (children reg y)) (set (children reg x)))
    x))

(defn classes

  "returns all classes for a type
   registry can be passed as first argument"

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
