(ns pby.types.builtins)

(do :help

    (defn- class-map [& xs]
      (reduce (fn [m [type class]]
                (assoc m type #{class}))
              {} (partition 2 xs)))

    (def key-set (comp set keys)))

(do :clj

    (def atoms
      (class-map
       :fun 'clojure.lang.Fn
       :num 'java.lang.Number
       :str 'java.lang.String
       :sym 'clojure.lang.Symbol
       :key 'clojure.lang.Keyword
       :entry 'clojure.lang.MapEntry))

    (def colls
      '{:seq #{clojure.lang.ISeq}
        :map #{clojure.lang.PersistentArrayMap
               clojure.lang.PersistentHashMap}
        :set #{clojure.lang.IPersistentSet}
        :vec #{clojure.lang.IPersistentVector}})

    (def prims
      (merge (class-map :nil nil)
             atoms colls))

    (def groups
      {:prim (key-set prims)
       :atom (key-set atoms)
       :coll (key-set colls)
       :word #{:key :str :sym}
       :line #{:vec :seq}
       :hash #{:map :set}}))

(do :cljs

    (def cljs-atoms
      (class-map
       :fun 'function
       :num 'number
       :str 'string
       :sym 'Symbol
       :key 'Keyword
       :entry 'MapEntry))

    (def cljs-colls
      '{:seq #{ArrayNodeSeq ChunkedCons ChunkedSeq
               Cons Cycle ES6IteratorSeq EmptyList
               IndexedSeq Iterate KeySeq LazySeq
               List NodeSeq PersistentArrayMapSeq
               PersistentQueue PersistentQueueSeq PersistentTreeMapSeq
               RSeq Range #_RangeChunk Repeat ValSeq}
        :map #{ObjMap PersistentArrayMap PersistentHashMap PersistentTreeMap}
        :set #{PersistentHashSet PersistentTreeSet}
        :vec #{BlackNode MapEntry PersistentVector RedNode Subvec}})

    (def cljs-prims
      (merge (class-map :nil nil)
             cljs-atoms cljs-colls))

    (def cljs-groups
      {:prim (key-set cljs-prims)
       :atom (key-set cljs-atoms)
       :coll (key-set cljs-colls)
       :word #{:key :str :sym}
       :line #{:vec :seq}
       :hash #{:map :set}}))

