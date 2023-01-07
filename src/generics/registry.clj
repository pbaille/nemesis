(ns generics.registry
  (:require [generics.state :as state]
            [generics.impl.utils :as u]))

(defn get-reg []
  (state/get :fns))

(defn get-spec [name]
  #_(println "resolve spec name " name (resolve name))
  (state/get-in [:fns (state/qualify-symbol name)]))

(defn get-spec! [name]
  #_(p/pprob @state)
  (or (get-spec name)
      (u/error "Cannot find spec " (pr-str name) "\nin\n" (u/pretty-str (keys (get-reg))))))

(defn reset! []
  (println "reset registry")
  (swap! state/state assoc-in [:clj :fns] {})
  (swap! state/state assoc-in [:cljs :fns] {}))

(defn conj! [spec]
  (state/swap! assoc-in [:fns (:fullname spec)] spec))