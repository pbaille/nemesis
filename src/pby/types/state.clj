(ns pby.types.state
  (:refer-clojure :exclude [get])
  (:require [pby.state :as state]))

(state/defstate :types)
