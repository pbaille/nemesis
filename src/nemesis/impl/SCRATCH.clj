(ns nemesis.impl.SCRATCH
  )

(defprotocol Yo
  (yo [x]))

(extend-protocol Yo
  PersistentVector
  (yo [v] (conj v :yopped)))

(satisfies? Yo [])