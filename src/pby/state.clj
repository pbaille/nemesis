(ns pby.state
  (:refer-clojure :exclude [get])
  (:require [pby.expansion :as exp]))

(def state (atom {:clj {} :cljs {}}))

(defn host-path [path]
  (into [(exp/host)] path))

(defmacro defstate

  ""

  [& segments]

  (let [subpath (vec segments)]

    `(do (defn ~'path [x#]
           (host-path
            (cond
              (keyword? x#) (conj ~subpath x#)
              (sequential? x#) (into ~subpath x#)
              :else (throw (Exception. (str "not a valid path " x#))))))

         ~'(defn get [x]
             (clojure.core/get-in @pby.state/state (path x)))

         ~'(defn upd!
             ([p f] (swap! pby.state/state clojure.core/update-in (path p) f))
             ([p f & pfs]
              (upd! p f)
              (doseq [[p f] (partition 2 pfs)]
                (upd! p f))))

         ~'(defn put!
             ([p v] (swap! pby.state/state clojure.core/assoc-in (path p) v))
             ([p v & pvs]
              (put! p v)
              (doseq [[p v] (partition 2 pvs)]
                (put! p v)))))))

(defstate)