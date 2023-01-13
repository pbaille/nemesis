(ns nemesis.tries.two
  (:require
    [nemesis.core :as g :include-macros true]
    [nemesis.tries.one :as one :include-macros true]))

#?(:clj (tap> "two"))


(g/deft trio [a b c]
  (one/->bub [_] (one/bub a b))
  (one/fmap [_ f] (trio (f a) (f b) (f c))))

(def t1
  (g/thing
   (one/fmap [_ f] (f 1))
   (one/->bub [_] (one/bub :iop :nop))
   (->trio [_] (trio :a :b :c))))

(assert
 (and

  (one/bub 1 1)
  (one/->bub 1)
  (one/->bub (one/bib 1))

  (= (one/bub 2 3)
     (one/->bub (one/fmap (trio 1 2 3) inc)))
  (= 2 (one/fmap t1 inc))
  (= (one/bub :iop :nop) (one/->bub t1))
  (= (trio :a :b :c) (->trio t1))))



(g/fork g1-clone1
      one/g1)

(assert (= (one/g1 "io") (g1-clone1 "io"))
        "fork base test")

(g/fork+ g1-clone
       one/g1
       [x]
       :sym "I sym")

(assert (= (g1-clone 'ert)
           "I sym")
        "fork+ base test")

(g/fork one/g1)

(assert (= (one/g1 'ert)
           (g1 'ert))
        "fork with same name")

(g/generic+ g1 [x]
            :sym "I sym 2")

(assert (= (g1 'ert)
           "I sym 2")
        "a fork is extensible")

(assert (= (one/g1 'ert)
           "I am key-or-sym")
        "an extended fork should not affect its parent")
