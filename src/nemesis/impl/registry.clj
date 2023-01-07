(ns nemesis.impl.registry
  (:require [nemesis.state :as state]
            [nemesis.types :as t]
            [nemesis.impl.utils :as u]))

(defn get-reg []
  (state/get :fns))

(defmacro display-reg []
  (list 'quote (state/get :fns)))

(defn get-spec [name]
  #_(println "resolve spec name " name (resolve name))
  (state/get-in [:fns (state/qualify-symbol name)]))

(defn get-spec! [name]
  #_(p/pprob @state)
  (or (get-spec name)
      (u/error "Cannot find spec " (pr-str name) "\nin\n" (u/pretty-str (keys (get-reg))))))

(defn reset-registry! []
  (println "reset-reg")
  (swap! state/state assoc-in [:clj :fns] {})
  (swap! state/state assoc-in [:cljs :fns] {}))

(defn register-spec! [spec]
  (state/swap! assoc-in [:fns (:fullname spec)] spec))

#_(defn conj-case
  [cases case]

  (let [ ;; any? #(= :any (:type %))
        same-arity? #(= (:arity case) (:arity %))
        parent? #(t/parentof (:type case) (:type %))
        overiden-case? #(and (same-arity? %) (parent? %) #_(p/prob :overiden %))
        remv (comp vec remove)]

    (conj (remv overiden-case? cases) case)

    #_(if (:default case)
      (if (or (and (any? case) (some any? cases))
              (some overiden-case? cases))
        cases
        (conj cases case))
      ;; overides
      (if (any? case)
        (conj (remv any? cases) case)
        (conj (remv overiden-case? cases) case)))))

(defn conj-case
  [cases {:as case :keys [type arity]}]

  (letfn [(overiden-case? [c]
            (and (= arity (:arity c))
                 (t/parentof type (:type c))))]
    (-> (remove overiden-case? cases)
        (vec)
        (conj case))))

(defn extension-cases
  [spec]
  (letfn [(expand-case [c]
            (map #(assoc c :class %) (t/classes (:type c))))
          (conj-case [m {:as case :keys [class type arity name]}]
            #_(assert (not (contains? m [class arity]))
                    (str "several cases for the same class and arity, spec is corrupted\n"
                          case))
            (assoc m [class arity]
                     (merge (-> spec :arities (get arity))
                            {:arity arity
                             :type type
                             :impl-name name})))]
    (->> (:cases spec)
         (mapcat expand-case)
         (reduce conj-case {})
         (map (fn [[[class arity] case]]
                (assoc case :class class :arity arity))))))

(defn extend-spec
  [spec extension-spec]

  (assert (every? (-> spec :arities keys set)
                  (-> extension-spec :arities keys))
          "unknown arity")

  (update spec :cases
          (fn [cs] (reduce conj-case cs (:cases extension-spec)))))
