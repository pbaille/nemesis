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

(defn conj-case
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

(defn extension-map [spec]
  (letfn [(expand-case [c]
            (map #(assoc c :class %) (t/classes (:type c))))
          (conj-case [m {:keys [class type arity name]}]
            (assoc m [class arity]
                     (merge (-> spec :arities (get arity))
                            {:arity arity
                             :type type
                             :impl-name name})))]
    (reduce conj-case {}
            (mapcat expand-case
                    (:cases spec)))))

(defn extend-spec
  [spec extension-spec]

  (assert (every? (or (-> spec :arities keys set)
                      (fn [x] (println "should not be here (generics/extend-spec") true))
                  (-> extension-spec :arities keys))
          "unknown arity")

  (update spec :cases
          (fn [cs] (reduce conj-case cs (:cases extension-spec)))))