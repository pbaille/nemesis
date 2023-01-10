(ns nemesis.tries.three
  (:require [nemesis.core :as g]
            [nemesis.tries.two :as two]))

(g/fork two/g1)

(g/generic+ g1
            [x]
            :str "overstr")

(g1 "qwe")
(g1 {})
