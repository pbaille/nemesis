(ns pby.types.guards
  (:require [pby.state :as state]))

(state/defstate :guards)

(def predicate-symbols
  {:fun `fn?
   :vec `vector?
   :seq `seq?
   :set `set?
   :map `map?
   :num `number?
   :key `keyword?
   :sym `symbol?
   :str `string?
   :nil `nil?})