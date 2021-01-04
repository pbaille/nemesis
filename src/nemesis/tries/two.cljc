(ns nemesis.tries.two
  (:require
    [nemesis.core :as g :refer [deft thing] :include-macros true]
    [nemesis.tries.one :as one]))

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
