(ns poly.functions.spec
  (:require [poly.utils.misc :as u]
            [poly.types.registry :as types]))

(defn merge-cases
  ""
  [original-spec extension-spec
   & {:keys [allow-default-overides]}]

  (assert (every? (-> original-spec :arities keys set)
                  (-> extension-spec :arities keys))
          "unknown arity extension.")

  (assert (or allow-default-overides
              (not (some :default (vals (:arities extension-spec)))))
          "default case can only be defined on generic creation (nemesis.core/defg)")

  (update original-spec :cases (partial concat (:cases extension-spec))))

(defn clone
  ""
  [{:as original-spec :keys [arities cases]}
   new-name]
  (let [{:as names :keys [protocol-prefix method-prefix]} (u/name_derive new-name)
        arities (into {} (map (fn [[arity spec]]
                                [arity (merge spec {:protocol-name (u/name_arify protocol-prefix arity)
                                                    :method-name (u/name_arify method-prefix arity)})])
                              arities))
        cases (map (fn [c] (assoc c :cloned true)) cases)]
    (merge original-spec
           names
           {:arities arities :cases cases :cloned-from (:fullname original-spec)})))

(defn implementers
  "given a generic spec,
       returns a vector of all types that implement the corresponding generic"
  [spec]
  (->> (:cases spec)
       (mapcat (fn [{t :type}] (if (set? t) t [t])))
       (into #{})))

(defn class-extensions
  [spec type-registry]
  (letfn [(expand-case [c]
               (map #(assoc c :class %) (types/classes type-registry (:type c))))
             (conj-case [m {:as _case :keys [class type arity name]}]
               (assoc m [class arity]
                      (merge (-> spec :arities (get arity))
                             {:arity arity
                              :class class
                              :type type
                              :impl-name name})))]
       (->> (reverse (:cases spec))
            (mapcat expand-case)
            (reduce conj-case {})
            (vals))))
