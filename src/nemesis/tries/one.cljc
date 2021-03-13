(ns nemesis.tries.one
  (:require [nemesis.core
             :as g
             :refer [deft defg generic+ type+ thing]
             :include-macros true]
            [nemesis.impl.registry :as r]))

(defg g1 [x]
      ;; prim type impl
      :vec "I am vec"
      ;; this type is a group
      ;; under the hood it implements for all collections
      :coll ["I am coll" x]
      ;; group litteral can be handy
      #{:key :sym} "I am key-or-sym")

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
       (g1 :a))))

;; extension
(generic+ g1 [x]
          ;; str impl
          :str ["str" x]
          ;; if a last expresion is given it extends Object
          :any [:unknown x])

(assert
  (and
    (g1 "a")
    (g/implements? (atom {}) g1)
    (g1 (atom {}))))


;; poly arity exemple
(defg g2
      ([x y]
       :vec [:g2vec x y]
       :num [:g2num x y]
       #{:sym :key} [:g2-sym-or-key x y]
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
          ([a b c & ds] :str [:variadstr a b c ds]))

;; extension of an existing generic

(assert
  (and
    (= (g2 [] 1)
       [:g2vec2 [] 1])
    (= (g2 "me" 1 2 3 4)
       [:variadstr "me" 1 2 '(3 4)])))

;; type+ is like extendtype
;; implement several generics at a time for a given type

(type+ :fun
       (g1 [x] :g1fun)
       (g2 [x y] (list :g2fun2 x y)))

(assert
  (and
    (= [:g2fun2 inc 1] (g2 inc 1))
    (= :g1fun (g1 (fn [a])))))

#_(p/error "stop")

(defg sip
      ([a b]
       :vec (conj a b)
       :map (apply assoc a b)
       :set (conj a b)
       :lst (concat a [b])
       :str (str a (.toString b))
       :sym (symbol (sip (name a) (.toString b)))
       :key (keyword (sip (name a) (.toString b))))
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
          [x] :key :validkey)

(assert
  (and
    (= :validkey (valid :a))
    (= :validword (valid 'a))))

(deft bub [x y])

(type+ :num
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


(defg pouet [x]
      :map [:map x]
      :hash [:hash x]
      [:default x]
      )

(require '[nemesis.impl.registry :as r]
         '[nemesis.impl.utils :as u])

(u/pp (r/get-spec 'pouet))

(pouet {:a 1 :b 2})
(pouet #{:a 1 :b 2})
(pouet 42)