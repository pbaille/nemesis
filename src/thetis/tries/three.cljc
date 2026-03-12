(ns thetis.tries.three
  (:require [thetis.core :as g :include-macros true]
            [thetis.tries.two :as two :include-macros true]))

(g/fork two/g1)

(g/generic+ g1
            [x]
            :string "overstr")

(assert (= "overstr" (g1 "qwe"))
        "forked g1 string override works")
(assert (= ["I am coll" {}] (g1 {}))
        "forked g1 inherits coll implementation")
