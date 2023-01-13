(ns nemesis.impl.registry
  (:require [nemesis.state :as state]
            [nemesis.types :as t]
            [nemesis.impl.utils :as u]))

(do :data

    (defn class-extensions
      "return a list cases objects (one for each class)
       taking case of potential overlaps.
       this can be used to emit 'extend-type or 'extend forms"
      [spec]
      (letfn [(expand-case [c]
                (map #(assoc c :class %) (t/classes (:type c))))
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

    (defn extend-spec
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

    (defn clone-spec
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
           (into #{}))))



(do :state

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

          (and (tap> @state/state)
               (u/error "Cannot find spec " (pr-str (state/qualify-symbol name)) "\nin\n" (keys (get-reg))))))

    (defn reset-registry! []
      (println "reset-reg")
      (swap! state/state assoc-in [:clj :fns] {})
      (swap! state/state assoc-in [:cljs :fns] {}))

    (defn register-spec! [spec]
      (state/swap! assoc-in [:fns (:fullname spec)] spec)
      spec)

    (defn extend-spec! [extension-spec]
      (register-spec!
       (extend-spec (get-spec! (:fullname extension-spec))
                    extension-spec)))

    (defn clone-spec! [original-name new-name]
      (register-spec!
       (clone-spec (get-spec! original-name)
                   new-name)))

    (defn implementers-map []
      (u/$vals (get-reg) implementers))

    (defn get-class-cases
      "get all generics implementations for the given class"
      [class]
      (mapv (fn [spec]
              {:spec spec
               :cases (filter (fn [case]
                                (= class (:class case)))
                              (class-extensions spec))})
            (vals (get-reg)))))






(comment :scratch

         (require '[nemesis.impl.parse :as p])
         (extend-spec (clone-spec (get-spec 'nemesis.tries.one/g1)
                                  'yo)
                      (p/parse '(yo [x] :set "I'm set" "overiden default"))
                      :allow-default-overides true))
