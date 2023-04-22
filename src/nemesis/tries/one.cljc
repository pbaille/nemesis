(ns nemesis.tries.one
  (:require [nemesis.core
             :as g
             :refer [defg generic+ type+ thing fork fork+ deft]
             :include-macros true]
            #?(:clj [nemesis.state :as state])
            #?(:clj [nemesis.impl.registry :as r])
            ))

#?(:clj (tap> "one"))

(comment
  (cljs.pprint/pprint (macroexpand '(defg gx [x] :map :vec [:any x])))
  (cljs.pprint/pprint (macroexpand '(defprotocol Io_1 (p_io_1 [x]))))
  (defprotocol Io_1 (p_io_1 [x]))
  (cljs.pprint/pprint (macroexpand '(extend-type function
                                      Io_1 (p_io_1 [f] :fun))))
  (cljs.pprint/pprint (macroexpand '(extend-type default
                                      Io_1 (p_io_1 [x] :any))))


  (defg gx [x] :map :vec [:any x])

  #_ (:cljs
      (do
        (println "hello")
        (comment
          (clojure.core/ns-unmap 'nemesis.tries.one 'gx)
          (clojure.core/ns-unmap 'nemesis.tries.one 'p_gx_1)
          (clojure.core/ns-unmap 'nemesis.tries.one 'Igx_1))
        (do (clojure.core/defprotocol Igx_1 (p_gx_1 [a_32526])))
        (clojure.core/defn gx ([a_32526] (p_gx_1 a_32526)))
        (do
          (clojure.core/swap!
           nemesis.core/prototypes
           clojure.core/assoc-in
           [:map 'nemesis.tries.one/gx 1]
           (clojure.core/fn ([x] :vec)))
          (clojure.core/swap!
           nemesis.core/prototypes
           clojure.core/assoc-in
           [:default 'nemesis.tries.one/gx 1]
           (clojure.core/fn ([x] [:any x]))))
        (println "here")
        (do
          (do
            (goog.object/set nemesis.tries.oneIgx_1 "_" true)
            (goog.object/set
             nemesis.tries.one.p_gx_1
             "_"
             (clojure.core/get-in
              @nemesis.core/prototypes
              [:default 'nemesis.tries.one/gx 1])))
          (println 2)
          (do
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             ObjMap
             "nemesis$tries$one$Igx_1$"
             cljs.core/PROTOCOL_SENTINEL)
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             ObjMap
             "nemesis$tries$one$Igx_1$p_gx_1$arity$1"
             (clojure.core/get-in
              @nemesis.core/prototypes
              [:map 'nemesis.tries.one/gx 1])))
          (println 3)
          (do
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentHashMap
             "nemesis$tries$one$Igx_1$"
             cljs.core/PROTOCOL_SENTINEL)
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentHashMap
             "nemesis$tries$one$Igx_1$p_gx_1$arity$1"
             (clojure.core/get-in
              @nemesis.core/prototypes
              [:map 'nemesis.tries.one/gx 1])))
          (println 4)
          (do
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentTreeMap
             "nemesis$tries$one$Igx_1$"
             cljs.core/PROTOCOL_SENTINEL)
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentTreeMap
             "nemesis$tries$one$Igx_1$p_gx_1$arity$1"
             (clojure.core/get-in
              @nemesis.core/prototypes
              [:map 'nemesis.tries.one/gx 1])))
          (println 5)
          (do
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentArrayMap
             "nemesis$tries$one$Igx_1$"
             cljs.core/PROTOCOL_SENTINEL)
            (js*
             "~{}[\"prototype\"][~{}] = ~{}"
             PersistentArrayMap
             "nemesis$tries$one$Igx_1$p_gx_1$arity$1"
             (clojure.core/get-in
              @nemesis.core/prototypes
              [:map 'nemesis.tries.one/gx 1]))))
        gx)))

(do :functions

    (defg g1 [x]
      ;; prim type impl
      :vec "I am vec"
      ;; this type is a group
      ;; under the hood it implements for all collections
      :coll ["I am coll" x]
      ;; group litteral can be handy
      #{:keyword :symbol} "I am key-or-sym"

      "Who am I ?")



    (assert
     (and
      (= "I am vec"
         (g1 []))
      (= ["I am coll" #{}]
         (g1 #{}))
      (= ["I am coll" '()]
         (g1 '()))
      (= "I am key-or-sym"
         (g1 'a)
         (g1 :a))
      (= "Who am I ?"
         (g1 1))))

    (defg nil-not-overiden-by-default [x]
      :nil :ok
      [:pouet x])

    (assert
     (and (= [:pouet true] (nil-not-overiden-by-default true))
          (= :ok (nil-not-overiden-by-default nil))))

    ;; extension
    (generic+ g1 [x]
              ;; str impl
              :string ["str" x])



    (assert
     (= (g1 "a")
        ["str" "a"]))

    ;; cannot overide or define the default implementation after creation

    #?(:clj (try (macroexpand '(generic+ g1 [x] "overiden default"))
                 (catch Throwable _ :catched)))

    ;; poly arity exemple
    (defg g2
      ([x y]
       :vec [:g2vec x y]
       :number [:g2num x y]
       #{:symbol :keyword} [:g2-sym-or-key x y]
       :coll [:g2coll x y]
       [:g2any x y])
      ([x y z]
       :coll [:coll x y z])
      ;; variadic arity
      ([x y z & more]
       [:variadic x y z more]))

    (assert
     (and
      (= (g2 [] 1)
         [:g2vec [] 1])
      (= (g2 #{} 1)
         [:g2coll #{} 1])
      (= (g2 #{} 1 2)
         [:coll #{} 1 2])
      (= (g2 "me" 1 2 3 4)
         [:variadic "me" 1 2 '(3 4)])
      (= (g2 :iop 1 2 3 4)
         [:variadic :iop 1 2 '(3 4)])))

    (generic+ g2
              ([a b] :vec [:g2vec2 a b])
              ([a b c & ds] :string [:variadstr a b c ds]))

    ;; extension of an existing generic

    (assert
     (and
      (= (g2 [] 1)
         [:g2vec2 [] 1])
      (= (g2 "me" 1 2 3 4)
         [:variadstr "me" 1 2 '(3 4)])))

    ;; several bindings for the same arity (here 1)

    (defg g3
      ([x] :number [:g3num x] [:g3default x])
      ([[x & xs]] :indexed [:g3line x xs]))


    (assert
     (and
      (= (g3 1)
         [:g3num 1])
      (= (g3 [1 2 3])
         [:g3line 1 '(2 3)])
      (= (g3 "a")
         [:g3default "a"])))

    ;; type+ is like extendtype
    ;; implement several generics at a time for a given type

    (type+ :function
           (g1 [x] :g1fun)
           (g2 [x y] (list :g2fun2 x y)))

    (assert
     (and
      (= [:g2fun2 inc 1] (g2 inc 1))
      (= :g1fun (g1 (fn [a])))))

    #_ (p/error "stop")

    (fork g1clone g1)

    (fork+ g2clone g2
           [x y]
           :string [:g2clone-str x y])

    (assert (and (= (g2clone "wer" 1)
                    [:g2clone-str "wer" 1])
                 (= (g2clone [] 2)
                    [:g2vec2 [] 2])
                 (let [a (atom nil)]
                   (= (g2clone a 1)
                      [:g2any a 1]))))

    (do :extras

        (defg sip
          ([a b]
           :vec (conj a b)
           :map (apply assoc a b)
           :set (conj a b)
           :seq (concat a [b])
           :string (str a (.toString b))
           :symbol (symbol (sip (name a) (.toString b)))
           :keyword (keyword (sip (name a) (.toString b))))
          ([a b & xs]
           (apply sip (sip a b) xs)))

        (assert
         (and
          (= (sip [] 1 2 3)
             [1 2 3])
          (= (sip #{} 1 2 3)
             #{1 2 3})))

        (defg valid
          [x]
          :nil nil
          :map (when (every? valid (vals x)) x)
          :coll (when (every? valid x) x)
          :word :validword
          x)

        (assert
         (and
          (not (valid [nil 1 nil]))
          (valid [1 2 3])
          (valid #{1 2 3})
          (valid {:a 1 :b 2})
          (not (valid {:a 1 :b 2 :c nil}))))

        (generic+ valid
                  [x] :keyword :validkey)

        (assert
         (and
          (= :validkey (valid :a))
          (= :validword (valid 'a))))))


(do :types

    (deft bub [x y])

    (type+ :number
           (->bub [x] (bub x x)))

    (defg fmap [x f])
    (defg greet [x])

    (deft bib [x]
      (->bub [_] (bub x x))
      (fmap [_ f] (bib (f x))))

    (assert
     (and
      (= (bub 1 1)
         (->bub 1))
      (= (fmap (bib 1) inc)
         (bib 2))
      (= (->bub (bib 1))
         (bub 1 1))))



    (deft point [x y]
      :belongs-to [:map]
      (g1 [x] "i'm a point"))



    (let [p (point 1 2)]

      (assert (= (g1 p) "i'm a point")
              "g1 impl is loaded")
      (assert (= (g2 p :pouet)
                 [:g2coll p :pouet])
              "g2 coll impl is inherited")
      (assert (= p (->point {:x 1 :y 2}))
              "cast function has been defined with a :map impl")

      (generic+ ->point [x]
                :vec (point (first x) (second x)))

      (assert (= p (->point [1 2]))
              "cast generic has been extended to vectors")))

(+ 1 2)
(tap> "one loaded")
