(ns thetis.tries.runner
  "Test runner that loads all tries namespaces.
   Top-level assertions in each namespace run at load time.
   If all pass, this script exits successfully."
  (:require [thetis.tries.one]
            [thetis.tries.two]
            [thetis.tries.three]
            [thetis.tries.four]
            [thetis.tries.five]))

(defn -main []
  (println "✓ All tries loaded — assertions passed."))
