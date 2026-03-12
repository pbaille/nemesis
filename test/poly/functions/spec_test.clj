(ns poly.functions.spec-test
  (:require [clojure.test :refer [deftest is testing]]
            [poly.functions.spec :as spec]
            [poly.functions.registry :as reg]
            [poly.functions.parse :as parse]))

;; === Helpers ===

(defn make-spec [name & cases-pairs]
  (parse/parse (cons name
                     (cons '[x]
                           cases-pairs))))

(defn make-ext-spec [name & cases-pairs]
  (parse/parse (cons name
                     (cons '[x]
                           cases-pairs))
               :extension-ns "test.ext"))

;; === Tests ===

(deftest init-spec-test
  (testing "init-spec sets up structured storage"
    (let [spec (make-spec 'foo :vec "vec" "default")
          init (spec/init-spec spec)]
      (is (= (:cases spec) (:declaration-cases init)))
      (is (= {} (:extension-cases init)))
      (is (= (:cases spec) (spec/effective-cases init))))))

(deftest merge-cases-with-ns-test
  (testing "merge-cases with extension-ns accumulates under namespace key"
    (let [orig (spec/init-spec (make-spec 'foo :vec "vec" "default"))
          ext1 (make-ext-spec 'foo :map "map")
          merged (spec/merge-cases orig ext1 "ns.b")]
      (is (contains? (:extension-cases merged) "ns.b"))
      (is (= 1 (count (get-in merged [:extension-cases "ns.b"]))))
      (is (= :map (:type (first (get-in merged [:extension-cases "ns.b"])))))))

  (testing "multiple extensions from same namespace accumulate"
    (let [orig (spec/init-spec (make-spec 'foo :vec "vec" "default"))
          ext1 (make-ext-spec 'foo :map "map")
          ext2 (make-ext-spec 'foo :set "set")
          merged1 (spec/merge-cases orig ext1 "ns.b")
          merged2 (spec/merge-cases merged1 ext2 "ns.b")]
      (is (= 2 (count (get-in merged2 [:extension-cases "ns.b"]))))
      (is (= [:map :set] (mapv :type (get-in merged2 [:extension-cases "ns.b"]))))))

  (testing "extensions from different namespaces are independent"
    (let [orig (spec/init-spec (make-spec 'foo :vec "vec" "default"))
          ext1 (make-ext-spec 'foo :map "map")
          ext2 (make-ext-spec 'foo :set "set")
          merged (-> orig
                     (spec/merge-cases ext1 "ns.b")
                     (spec/merge-cases ext2 "ns.c"))]
      (is (= 1 (count (get-in merged [:extension-cases "ns.b"]))))
      (is (= 1 (count (get-in merged [:extension-cases "ns.c"])))))))

(deftest effective-cases-ordering-test
  (testing "extension cases precede declaration cases"
    (let [orig (spec/init-spec (make-spec 'foo :vec "vec" "default"))
          ext (make-ext-spec 'foo :map "map")
          merged (spec/merge-cases orig ext "ns.b")
          eff (spec/effective-cases merged)]
      ;; Extension cases first, then declaration cases
      (is (= :map (:type (first eff))))
      (is (= :default (:type (last eff))))))

  (testing "later namespace keys have higher precedence"
    (let [orig (spec/init-spec (make-spec 'foo "default"))
          ext1 (make-ext-spec 'foo :map "map")
          ext2 (make-ext-spec 'foo :set "set")
          merged (-> orig
                     (spec/merge-cases ext1 "ns.a")
                     (spec/merge-cases ext2 "ns.b"))
          eff (spec/effective-cases merged)
          types (mapv :type eff)]
      ;; ns.b is later alphabetically → higher precedence → comes first
      (is (= :set (first types)))
      (is (= :map (second types))))))

(deftest idempotent-rebuild-test
  (testing "simulated incremental build: remove + recompile produces same state"
    (let [;; Initial state: defg in ns-a, generic+ in ns-b
          reg0 {}
          spec-a (make-spec 'foo :vec "vec" "default")
          ext-b (make-ext-spec 'foo :map "map")

          ;; First build
          reg1 (reg/add-spec reg0 spec-a)
          reg2 (reg/extend-spec reg1 ext-b "ns.b")

          ;; Capture the state
          spec-after-first-build (get reg2 'user/foo)

          ;; Simulate incremental rebuild of ns.b:
          ;; 1. Remove ns.b contributions
          reg-cleaned (reg/remove-ns-contributions reg2 "ns.b")
          ;; 2. Recompile ns.b (same extension)
          reg-rebuilt (reg/extend-spec reg-cleaned ext-b "ns.b")

          spec-after-rebuild (get reg-rebuilt 'user/foo)]

      ;; The specs should be identical
      (is (= (:cases spec-after-first-build) (:cases spec-after-rebuild)))
      (is (= (:extension-cases spec-after-first-build) (:extension-cases spec-after-rebuild)))
      (is (= (:declaration-cases spec-after-first-build) (:declaration-cases spec-after-rebuild))))))

(deftest no-duplication-on-rebuild-test
  (testing "rebuilding a namespace doesn't duplicate its cases"
    (let [reg0 {}
          spec-a (make-spec 'foo :vec "vec" "default")
          ext-b1 (make-ext-spec 'foo :map "map1")
          ext-b2 (make-ext-spec 'foo :set "set1")

          ;; First build: ns-a defines, ns-b extends twice (two generic+ calls)
          reg1 (-> reg0
                   (reg/add-spec spec-a)
                   (reg/extend-spec ext-b1 "ns.b")
                   (reg/extend-spec ext-b2 "ns.b"))

          first-case-count (count (:cases (get reg1 'user/foo)))

          ;; Rebuild ns.b: clean then re-extend
          reg2 (-> reg1
                   (reg/remove-ns-contributions "ns.b")
                   (reg/extend-spec ext-b1 "ns.b")
                   (reg/extend-spec ext-b2 "ns.b"))

          second-case-count (count (:cases (get reg2 'user/foo)))]

      (is (= first-case-count second-case-count)
          "Case count should be identical after rebuild"))))

(deftest clone-snapshots-effective-cases-test
  (testing "clone snapshots effective cases, isolating from future extensions"
    (let [reg0 {}
          spec-a (make-spec 'foo :vec "vec" "default")
          ext-b (make-ext-spec 'foo :map "map")

          ;; Build: define + extend
          reg1 (-> reg0
                   (reg/add-spec spec-a)
                   (reg/extend-spec ext-b "ns.b"))

          ;; Clone
          reg2 (reg/clone-spec reg1 'user/foo 'user/foo-clone)

          ;; Extend original further
          ext-c (make-ext-spec 'foo :set "set")
          reg3 (reg/extend-spec reg2 ext-c "ns.c")

          original (get reg3 'user/foo)
          clone (get reg3 'user/foo-clone)]

      ;; Clone should NOT have the :set extension
      (is (some #(= :set (:type %)) (spec/effective-cases original)))
      (is (not (some #(= :set (:type %)) (spec/effective-cases clone)))))))

(deftest remove-ns-contributions-test
  (testing "removing ns contributions clears extensions from that ns"
    (let [reg0 {}
          spec-a (make-spec 'foo :vec "vec" "default")
          ext-b (make-ext-spec 'foo :map "map")
          ext-c (make-ext-spec 'foo :set "set")

          reg1 (-> reg0
                   (reg/add-spec spec-a)
                   (reg/extend-spec ext-b "ns.b")
                   (reg/extend-spec ext-c "ns.c"))

          ;; Remove ns.b contributions
          reg2 (reg/remove-ns-contributions reg1 "ns.b")
          spec (get reg2 'user/foo)]

      (is (nil? (get-in spec [:extension-cases "ns.b"])))
      (is (some? (get-in spec [:extension-cases "ns.c"])))
      (is (= 3 (count (:cases spec)))) ;; :set + :vec + default
      ))

  (testing "removing ns that defined a spec removes the spec entirely"
    (let [reg0 {}
          spec-a (parse/parse '(foo [x] :vec "vec" "default"))
          reg1 (reg/add-spec reg0 spec-a)
          reg2 (reg/remove-ns-contributions reg1 "user")]

      (is (nil? (get reg2 'user/foo))))))
