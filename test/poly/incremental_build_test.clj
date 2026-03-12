(ns poly.incremental-build-test
  "Tests that simulate incremental build scenarios.
   These validate that the namespace-keyed spec registration model
   enables correct incremental builds."
  (:require [clojure.test :refer [deftest is testing]]
            [poly.functions.spec :as fn.spec]
            [poly.functions.registry :as fn.reg]
            [poly.functions.parse :as parse]
            [poly.compiler.core :as compiler]
            [poly.compiler.data :as compiler-data]
            [poly.types.core :as types]
            [poly.types.data :as type-data]))

;; NOTE: parse/parse always uses *ns* (= user in these tests) for :ns and :fullname.
;; Extension-ns is separate — it's the namespace contributing the extension.
;; So specs are always user/NAME, and extension-ns is the contributing ns.

;; === Helpers ===

(defn parse-defg [name & body]
  (parse/parse (cons name body)))

(defn parse-ext [name & body]
  ;; extension-ns is set to a dummy; the actual ns-key is passed to compiler/extend-function
  (parse/parse (cons name body) :extension-ns 'ext))

;; === Tests ===

(deftest full-incremental-build-scenario
  (testing "Scenario: ns-a defines generic, ns-b and ns-c extend it.
            Rebuild ns-b only → state is correct."
    (let [;; defg greet [x] :vec [:greet-vec x] [:greet-default x]
          spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ;; generic+ greet [x] :map [:greet-map x]
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])
          ;; generic+ greet [x] :string [:greet-str x]
          ext-c (parse-ext 'greet '[x] :string '[:greet-str x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b "ns.b")
                 (compiler/extend-function ext-c "ns.c"))

          original-spec (compiler/get-function c1 'user/greet)
          original-cases (fn.spec/effective-cases original-spec)
          original-types (mapv :type original-cases)]

      ;; 4 cases: :string (ns.c), :map (ns.b), :vec (decl), :default (decl)
      (is (= 4 (count original-cases)))
      (is (= [:string :map :vec :default] original-types))

      ;; === INCREMENTAL REBUILD: ns-b changes ===
      ;; ns-b now: (generic+ greet [x] :map [:greet-map-v2 x] :set [:greet-set x])
      (let [ext-b-v2 (parse-ext 'greet '[x] :map '[:greet-map-v2 x] :set '[:greet-set x])
            c2 (compiler/remove-ns-contributions c1 "ns.b")
            c3 (compiler/extend-function c2 ext-b-v2 "ns.b")
            rebuilt-spec (compiler/get-function c3 'user/greet)
            rebuilt-cases (fn.spec/effective-cases rebuilt-spec)
            rebuilt-types (set (map :type rebuilt-cases))]

        ;; 5 cases: :string (ns.c), :map + :set (ns.b v2), :vec + :default (decl)
        (is (= 5 (count rebuilt-cases)))
        (is (contains? rebuilt-types :string))
        (is (contains? rebuilt-types :set))
        (is (contains? rebuilt-types :map))
        (is (contains? rebuilt-types :vec))
        (is (contains? rebuilt-types :default))))))

(deftest deleted-definition-scenario
  (testing "Scenario: ns-b had a generic+ but the extension is deleted."
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b "ns.b"))

          ;; ns.b is edited: generic+ line deleted
          c2 (compiler/remove-ns-contributions c1 "ns.b")

          spec-after (compiler/get-function c2 'user/greet)
          types (mapv :type (fn.spec/effective-cases spec-after))]

      ;; Only declaration cases remain
      (is (= [:vec :default] types)))))

(deftest fork-survives-rebuild-test
  (testing "Forked specs survive when original's extensions are rebuilt"
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b "ns.b"))

          ;; Fork greet → my-greet
          c2 (compiler/clone-function c1 'user/greet 'user/my-greet)
          fork-spec (compiler/get-function c2 'user/my-greet)
          fork-types (set (map :type (fn.spec/effective-cases fork-spec)))

          ;; Rebuild ns.b with different extension
          ext-b-v2 (parse-ext 'greet '[x] :string '[:greet-str x])
          c3 (-> c2
                 (compiler/remove-ns-contributions "ns.b")
                 (compiler/extend-function ext-b-v2 "ns.b"))

          fork-after (compiler/get-function c3 'user/my-greet)
          fork-types-after (set (map :type (fn.spec/effective-cases fork-after)))]

      ;; Fork should have :map (from clone time)
      (is (contains? fork-types :map))
      ;; Fork should NOT be affected by rebuild
      (is (= fork-types fork-types-after))
      ;; Fork should NOT have :string (added after clone)
      (is (not (contains? fork-types-after :string))))))

(deftest double-compile-without-clean-test
  (testing "Compiling a namespace twice without cleaning duplicates cases"
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b "ns.b"))

          count-before (count (fn.spec/effective-cases
                               (compiler/get-function c1 'user/greet)))

          ;; Without cleaning, re-extend (this is the bug incremental builds must avoid)
          c2 (compiler/extend-function c1 ext-b "ns.b")
          count-after (count (fn.spec/effective-cases
                              (compiler/get-function c2 'user/greet)))]

      ;; Without cleaning, cases accumulate under same ns key
      (is (> count-after count-before)
          "Without remove-ns-contributions, re-extension duplicates cases")))

  (testing "Clean + recompile is idempotent"
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b "ns.b"))

          cases-before (fn.spec/effective-cases
                        (compiler/get-function c1 'user/greet))

          ;; Clean then re-extend
          c2 (-> c1
                 (compiler/remove-ns-contributions "ns.b")
                 (compiler/extend-function ext-b "ns.b"))

          cases-after (fn.spec/effective-cases
                       (compiler/get-function c2 'user/greet))]

      (is (= (count cases-before) (count cases-after))
          "Clean + recompile produces identical case count")
      (is (= (mapv :type cases-before) (mapv :type cases-after))
          "Clean + recompile produces identical case types in same order"))))

(deftest remove-defining-ns-test
  (testing "Removing the namespace that defined the spec removes it entirely"
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          c0 (compiler-data/make-compiler)
          c1 (compiler/add-function c0 spec-a)

          ;; parse uses *ns* = user, so :ns = user
          c2 (compiler/remove-ns-contributions c1 "user")]

      (is (nil? (compiler/get-function c2 'user/greet))))))

(deftest multiple-extensions-same-ns-test
  (testing "Multiple generic+ calls from same namespace accumulate correctly"
    (let [spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ext-b1 (parse-ext 'greet '[x] :map '[:greet-map x])
          ext-b2 (parse-ext 'greet '[x] :set '[:greet-set x])

          c0 (compiler-data/make-compiler)
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (compiler/extend-function ext-b1 "ns.b")
                 (compiler/extend-function ext-b2 "ns.b"))

          spec (compiler/get-function c1 'user/greet)
          types (mapv :type (fn.spec/effective-cases spec))]

      ;; Both extensions from ns.b should be present
      (is (= 4 (count types)))
      (is (some #(= :map %) types))
      (is (some #(= :set %) types))

      ;; Clean and re-extend produces same result
      (let [c2 (-> c1
                   (compiler/remove-ns-contributions "ns.b")
                   (compiler/extend-function ext-b1 "ns.b")
                   (compiler/extend-function ext-b2 "ns.b"))
            types2 (mapv :type (fn.spec/effective-cases
                                (compiler/get-function c2 'user/greet)))]
        (is (= types types2))))))

;; === Type Registry Incremental Build Tests ===

(defn make-compiler-with-type-state
  "Create a compiler with namespace-keyed type state."
  []
  (let [base-types (merge type-data/clj-base-types type-data/groups)]
    (assoc (compiler-data/make-compiler base-types)
           :type-state (types/make-type-state base-types))))

(deftest type-registry-incremental-build-test
  (testing "Scenario: ns-a registers a type, ns-a is rebuilt → types are correct"
    (let [c0 (make-compiler-with-type-state)
          ;; ns-a registers :point type belonging to :map group
          ts1 (types/register-type-contribution (:type-state c0) "ns.a" :point '#{Point} [:map])
          c1 (assoc c0 :type-state ts1 :types (types/effective-types ts1))

          ;; Verify type is registered
          _ (is (= '#{Point} (get (:types c1) :point)))
          _ (is (contains? (get (:types c1) :map) :point))

          ;; Rebuild ns.a: clean + re-register
          c2 (compiler/remove-ns-contributions c1 "ns.a")

          ;; Type should be gone after cleanup
          _ (is (nil? (get (:types c2) :point)))
          _ (is (not (contains? (get (:types c2) :map) :point)))

          ;; Re-register
          ts3 (types/register-type-contribution (:type-state c2) "ns.a" :point '#{Point} [:map])
          c3 (assoc c2 :type-state ts3 :types (types/effective-types ts3))]

      ;; Types should be back
      (is (= '#{Point} (get (:types c3) :point)))
      (is (contains? (get (:types c3) :map) :point))
      ;; Base types untouched
      (is (contains? (:types c3) :vec))
      (is (contains? (:types c3) :coll))))

  (testing "Scenario: ns-a and ns-b register types, rebuild ns-a → ns-b untouched"
    (let [c0 (make-compiler-with-type-state)
          ts1 (-> (:type-state c0)
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map])
                  (types/register-type-contribution "ns.b" :color '#{Color} [:coll]))
          c1 (assoc c0 :type-state ts1 :types (types/effective-types ts1))

          ;; Rebuild ns.a only
          c2 (compiler/remove-ns-contributions c1 "ns.a")]

      ;; ns.a's :point should be gone
      (is (nil? (get (:types c2) :point)))
      ;; ns.b's :color should remain
      (is (= '#{Color} (get (:types c2) :color)))
      (is (contains? (get (:types c2) :coll) :color))))

  (testing "Scenario: ns-a changes type definition on rebuild"
    (let [c0 (make-compiler-with-type-state)
          ts1 (types/register-type-contribution (:type-state c0) "ns.a" :point '#{Point2D} [:map])
          c1 (assoc c0 :type-state ts1 :types (types/effective-types ts1))

          ;; ns-a is rebuilt: now :point is Point3D
          c2 (compiler/remove-ns-contributions c1 "ns.a")
          ts3 (types/register-type-contribution (:type-state c2) "ns.a" :point '#{Point3D} [:map])
          c3 (assoc c2 :type-state ts3 :types (types/effective-types ts3))]

      ;; Should have new class, not old
      (is (= '#{Point3D} (get (:types c3) :point)))
      (is (not (contains? (get (:types c3) :point) 'Point2D)))))

  (testing "Combined function + type rebuild"
    (let [c0 (make-compiler-with-type-state)
          ;; defg in ns.a
          spec-a (parse-defg 'greet '[x] :vec '[:greet-vec x] '[:greet-default x])
          ;; type in ns.b
          ts1 (types/register-type-contribution (:type-state c0) "ns.b" :point '#{Point} [:map])
          c1 (-> c0
                 (compiler/add-function spec-a)
                 (assoc :type-state ts1 :types (types/effective-types ts1)))

          ;; Rebuild ns.b: both function extensions and types should be cleaned
          ext-b (parse-ext 'greet '[x] :map '[:greet-map x])
          c2 (-> c1 (compiler/extend-function ext-b "ns.b"))

          ;; Now rebuild ns.b
          c3 (compiler/remove-ns-contributions c2 "ns.b")]

      ;; Function extensions from ns.b should be gone
      (is (= [:vec :default]
             (mapv :type (fn.spec/effective-cases
                          (compiler/get-function c3 'user/greet)))))
      ;; Type from ns.b should be gone
      (is (nil? (get (:types c3) :point))))))
