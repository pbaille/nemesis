(ns poly.state
  (:refer-clojure :exclude [reset! swap! get get-in])
  (:require [clojure.core :as c]
            [poly.utils.expansion :as exp]))

(def state0
  (let [base {:functions {}
              :types {}
              :guards {}
              :namespaces {}}]
    {:clj base
     :cljs base}))

(def state (atom state0))

(defn clj-state [] (:clj @state))
(defn cljs-state [] (:cljs @state))

(defn current []
  (if (exp/cljs?) (cljs-state) (clj-state)))

(defn get
  ([] (current))
  ([k] (c/get (current) k)))

(defn get-in [p]
  (c/get-in (current) p))

(defn compilation-target []
  (if (exp/cljs?) :cljs :clj))

(defn swap! [f & args]
  (c/swap! state update (compilation-target) #(apply f % args)))

(defn reset!
  ([] (reset! state0))
  ([x] (clojure.core/reset! state x)))
