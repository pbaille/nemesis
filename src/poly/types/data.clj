(ns poly.types.data)

(def clj-base-types
  '{:nil #{nil}
    :function #{clojure.lang.Fn}
    :number #{java.lang.Number}
    :string #{java.lang.String}
    :symbol #{clojure.lang.Symbol}
    :boolean #{clojure.lang.Boolean}
    :keyword #{clojure.lang.Keyword}
    :map-entry #{clojure.lang.MapEntry}
    :seq #{clojure.lang.ISeq}
    :map #{clojure.lang.PersistentArrayMap
           clojure.lang.PersistentHashMap}
    :set #{clojure.lang.IPersistentSet}
    :vec #{clojure.lang.IPersistentVector}
    :default #{java.lang.Object nil}})

(def cljs-base-types
  '{:nil #{nil}
    :function #{function}
    :number #{number}
    :string #{string}
    :boolean #{boolean}
    :symbol #{Symbol}
    :keyword #{Keyword}
    :map-entry #{MapEntry}
    :seq #{ArrayNodeSeq ChunkedCons ChunkedSeq
           Cons Cycle ES6IteratorSeq EmptyList
           IndexedSeq IntegerRange IntegerRangeChunk Iterate KeySeq LazySeq
           List NodeSeq PersistentArrayMapSeq
           PersistentQueue PersistentQueueSeq PersistentTreeMapSeq
           RSeq Range Repeat ValSeq}
    :map #{ObjMap PersistentArrayMap PersistentHashMap PersistentTreeMap}
    :set #{PersistentHashSet PersistentTreeSet}
    :vec #{BlackNode MapEntry PersistentVector RedNode Subvec}
    :default #{default}})

(def groups
  {:coll #{:map :set :seq :vec}
   :word #{:keyword :string :symbol}
   :indexed #{:vec :seq}
   :hashed #{:map :set}
   :builtin (disj (set (keys clj-base-types)) :default)})

(def preds-symbols
  {:function `fn?
   :vec `vector?
   :seq `seq?
   :set `set?
   :map `map?
   :number `number?
   :keyword `keyword?
   :symbol `symbol?
   :string `string?
   :boolean `boolean?
   :nil `nil?})
