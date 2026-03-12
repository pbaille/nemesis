(ns thetis.tries.five
  (:require [thetis.core
             :as g
             :refer [defg generic+ fork fork+ defguard]
             :include-macros true]
            #?(:clj [thetis.state :as state])))

;; === Predicate Guards ===

;; Define a guard type — :positive is a number that's positive
(defguard :positive
  :base :number
  :pred pos?)

;; Define another guard
(defguard :non-empty-vec
  :base :vec
  :pred seq)

;; A generic using guard types
(defg describe [x]
  :positive     [:pos x]
  :non-empty-vec [:nev x]
  :number       [:num x]
  :vec          [:vec x]
  [:default x])

(assert (= [:pos 42] (describe 42)))
(assert (= [:num -3] (describe -3)))
(assert (= [:num 0] (describe 0)))
(assert (= [:nev [1 2]] (describe [1 2])))
(assert (= [:vec []] (describe [])))
(assert (= [:default "hello"] (describe "hello")))

;; Guards should work with generic+
(defguard :non-empty-string
  :base :string
  :pred seq)

(generic+ describe [x]
  :non-empty-string [:nes x])

(assert (= [:nes "hello"] (describe "hello")))
(assert (= [:default ""] (describe "")))

;; Guards should work with extension modes
;; :positive is a child of :number, so under :refine mode,
;; adding :positive when :number has an impl is specialization (allowed)
(defg ^{:mode :refine} safe-desc [x]
  :number [:num x]
  [:default x])

(generic+ safe-desc [x]
  :positive [:pos x])

(assert (= [:pos 5] (safe-desc 5)))
(assert (= [:num -1] (safe-desc -1)))

;; Under :extend mode, specialization should be blocked
(defg ^{:mode :extend} strict-desc [x]
  :number [:num x]
  [:default x])

#?(:clj
   (assert
    (try
      (macroexpand '(thetis.core/generic+ thetis.tries.five/strict-desc [x]
                      :positive [:pos x]))
      false
      (catch Throwable e
        (let [root (loop [e e] (if-let [c (.getCause e)] (recur c) e))]
          (boolean (re-find #"extend" (str (.getMessage root)))))))
    "extend mode should block guard specialization"))

;; Multi-arity with guards
(defg multi-g
  ([x]
   :positive [:pos1 x]
   :number   [:num1 x]
   [:default1 x])
  ([x y]
   :positive [:pos2 x y]
   [:default2 x y]))

(assert (= [:pos1 5] (multi-g 5)))
(assert (= [:num1 -1] (multi-g -1)))
(assert (= [:pos2 3 :a] (multi-g 3 :a)))
(assert (= [:default2 -1 :a] (multi-g -1 :a)))

;; Fork should copy guard cases
(g/fork describe2 describe)

(assert (= [:pos 42] (describe2 42)))
(assert (= [:nev [1]] (describe2 [1])))
(assert (= [:num -5] (describe2 -5)))
(assert (= [:vec []] (describe2 [])))

;; Fork+ should also work
(fork+ describe3 describe
  [x] :number [:num-override x])

;; :number override applies (fork resets to :override mode)
(assert (= [:num-override -1] (describe3 -1)))
;; But :positive guard still fires for positive numbers
;; because guards are checked before protocol dispatch
(assert (= [:pos 42] (describe3 42)))

;; === Guards with set types ===
(defguard :non-empty-coll
  :base :coll
  :pred seq)

(defg coll-desc [x]
  :non-empty-coll [:nec x]
  :coll [:empty-coll x]
  [:other x])

(assert (= [:nec [1 2 3]] (coll-desc [1 2 3])))
(assert (= [:nec {:a 1}] (coll-desc {:a 1})))
(assert (= [:empty-coll []] (coll-desc [])))
(assert (= [:empty-coll #{}] (coll-desc #{})))
(assert (= [:other 42] (coll-desc 42)))

(println "Thetis Predicate Guards: ALL TESTS OK")
