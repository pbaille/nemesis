(ns thetis.tries.four
  (:require [thetis.core
             :as g
             :refer [defg generic+ type+ thing fork fork+ deft]
             :include-macros true]
            #?(:clj [thetis.state :as state])))

;; Test extension modes

;; A generic with :extend mode — no overrides allowed
(defg ^{:mode :extend} safe-fn [x]
  :vec [:safe-vec x]
  [:safe-default x])

;; Check the spec was stored with :extend mode
#?(:clj
   (do
     (let [spec (get (state/get :functions) 'thetis.tries.four/safe-fn)]
       (assert (= :extend (:mode spec))
               "spec should have :extend mode"))))

;; Extending with a new type should work
(generic+ safe-fn [x]
  :map [:safe-map x])

(assert (= [:safe-vec [1 2]] (safe-fn [1 2])))
(assert (= [:safe-map {:a 1}] (safe-fn {:a 1})))
(assert (= [:safe-default 42] (safe-fn 42)))

;; Attempting to override :vec should throw at compile time
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/safe-fn [x]
                      :vec [:overridden x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (println "Root cause:" (.getMessage root))
          (boolean (re-find #"extend" (str (.getMessage root)))))))
    "extend mode should block override"))

;; A generic with explicit :override mode — overrides work
(defg ^{:mode :override} patchable-fn [x]
  :vec [:patch-vec x]
  [:patch-default x])

(generic+ patchable-fn [x]
  :vec [:patched-vec x])

(assert (= [:patched-vec [1]] (patchable-fn [1])))

;; Test that :extend mode allows extending with new types (including set types)
(defg ^{:mode :extend} multi-fn [x]
  :vec [:multi-vec x]
  [:multi-default x])

(generic+ multi-fn [x]
  #{:keyword :symbol} [:multi-word x])

(assert (= [:multi-vec []] (multi-fn [])))
(assert (= [:multi-word :a] (multi-fn :a)))
(assert (= [:multi-word 'b] (multi-fn 'b)))
(assert (= [:multi-default 42] (multi-fn 42)))

;; Test that :extend blocks override of set types too
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/multi-fn [x]
                      #{:keyword :symbol} [:overridden x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"extend" (str (.getMessage root)))))))))

;; Test that :extend blocks specialization (adding :vec when :coll covers it)
(defg ^{:mode :extend} extend-coll-fn [x]
  :coll [:extend-coll x]
  [:extend-default x])

#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/extend-coll-fn [x]
                      :vec [:specialize-vec x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"extend" (str (.getMessage root)))))))
    "extend mode should block specialization of parent types"))

;; === :refine mode ===
;; :refine allows specialization (adding :vec when :coll has impl)
;; but blocks direct overrides

(defg ^{:mode :refine} refinable-fn [x]
  :coll [:refine-coll x]
  [:refine-default x])

;; Specializing :vec under :coll should work — this is the core :refine feature
(generic+ refinable-fn [x]
  :vec [:refine-vec x])

(assert (= [:refine-vec [1 2]] (refinable-fn [1 2])))
(assert (= [:refine-coll {:a 1}] (refinable-fn {:a 1})))
(assert (= [:refine-coll #{1}] (refinable-fn #{1})))
(assert (= [:refine-default 42] (refinable-fn 42)))

;; Adding a completely new type should work (same as :extend behavior)
(generic+ refinable-fn [x]
  :number [:refine-num x])

(assert (= [:refine-num 42] (refinable-fn 42)))

;; Attempting to directly override :vec (which now has its own impl) should throw
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/refinable-fn [x]
                      :vec [:override-vec x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"refine" (str (.getMessage root)))))))
    "refine mode should block direct override of existing type"))

;; Attempting to directly override :coll (which has a direct impl) should throw
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/refinable-fn [x]
                      :coll [:override-coll x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"refine" (str (.getMessage root)))))))
    "refine mode should block direct override of :coll"))

;; === :sealed mode ===
;; No extensions at all

(defg ^{:mode :sealed} locked-fn [x]
  :vec [:locked-vec x]
  [:locked-default x])

(assert (= [:locked-vec [1]] (locked-fn [1])))
(assert (= [:locked-default 42] (locked-fn 42)))

;; Any extension should throw
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/locked-fn [x]
                      :map [:locked-map x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"sealed" (str (.getMessage root)))))))
    "sealed mode should block all extensions"))

;; === Default mode test ===
;; No mode metadata → default is :refine

(defg default-mode-fn [x]
  :coll [:dm-coll x]
  [:dm-default x])

;; Check that the spec has :refine mode
#?(:clj
   (do
     (let [spec (get (state/get :functions) 'thetis.tries.four/default-mode-fn)]
       (assert (= :refine (:mode spec))
               "default mode should be :refine"))))

;; Specialization should work (refine behavior)
(generic+ default-mode-fn [x]
  :vec [:dm-vec x])

(assert (= [:dm-vec [1]] (default-mode-fn [1])))
(assert (= [:dm-coll {:a 1}] (default-mode-fn {:a 1})))

;; Direct override should be blocked (refine behavior)
#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.four/default-mode-fn [x]
                      :vec [:dm-override x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"refine" (str (.getMessage root)))))))
    "default mode (refine) should block direct overrides"))

;; === Fork mode reset test ===
;; Fork of any mode should reset to :override

(defg ^{:mode :sealed} sealed-parent [x]
  :vec [:sp-vec x]
  [:sp-default x])

(fork unsealed-child sealed-parent)

;; The fork should have :override mode despite parent being :sealed
#?(:clj
   (do
     (let [spec (get (state/get :functions) 'thetis.tries.four/unsealed-child)]
       (assert (= :override (:mode spec))
               "forked spec should have :override mode"))))

;; And we should be able to extend it freely
(generic+ unsealed-child [x]
  :map [:uc-map x])

(assert (= [:sp-vec [1]] (unsealed-child [1])))
(assert (= [:uc-map {:a 1}] (unsealed-child {:a 1})))

;; Can even override existing implementations on the fork
(generic+ unsealed-child [x]
  :vec [:uc-vec x])

(assert (= [:uc-vec [1]] (unsealed-child [1])))

(println "Thetis Extension modes: ALL TESTS OK")
