(ns poly.types.core-test
  "Tests for namespace-keyed type contributions in poly.types.core."
  (:require [clojure.test :refer [deftest is testing]]
            [poly.types.core :as types]
            [poly.types.data :as type-data]))

;; === Helpers ===

(defn base-type-state []
  (types/make-type-state (merge type-data/clj-base-types type-data/groups)))

;; === Tests ===

(deftest make-type-state-test
  (testing "make-type-state wraps a base registry"
    (let [ts (base-type-state)]
      (is (map? (:base ts)))
      (is (= {} (:contributions ts)))
      (is (contains? (:base ts) :vec))
      (is (contains? (:base ts) :coll)))))

(deftest effective-types-with-no-contributions-test
  (testing "effective-types with no contributions returns base"
    (let [ts (base-type-state)]
      (is (= (:base ts) (types/effective-types ts))))))

(deftest register-type-contribution-test
  (testing "registering a type contribution adds tags and groups"
    (let [ts (base-type-state)
          ts2 (types/register-type-contribution ts "ns.a" :my-point '#{MyPoint} [:map])]
      ;; Contribution is stored under namespace key
      (is (= #{:my-point} (get-in ts2 [:contributions "ns.a" :groups :map])))
      (is (= '#{MyPoint} (get-in ts2 [:contributions "ns.a" :tags :my-point])))
      ;; Effective types include the new type
      (let [eff (types/effective-types ts2)]
        (is (= '#{MyPoint} (get eff :my-point)))
        (is (contains? (get eff :map) :my-point)))))

  (testing "multiple registrations from same namespace accumulate"
    (let [ts (base-type-state)
          ts2 (-> ts
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map])
                  (types/register-type-contribution "ns.a" :line '#{Line} [:coll]))]
      (is (= '#{Point} (get-in ts2 [:contributions "ns.a" :tags :point])))
      (is (= '#{Line} (get-in ts2 [:contributions "ns.a" :tags :line])))
      (let [eff (types/effective-types ts2)]
        (is (contains? (get eff :map) :point))
        (is (contains? (get eff :coll) :line)))))

  (testing "registrations from different namespaces are independent"
    (let [ts (base-type-state)
          ts2 (-> ts
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map])
                  (types/register-type-contribution "ns.b" :line '#{Line} [:vec]))]
      (is (some? (get-in ts2 [:contributions "ns.a"])))
      (is (some? (get-in ts2 [:contributions "ns.b"])))
      (let [eff (types/effective-types ts2)]
        (is (contains? (get eff :map) :point))
        (is (contains? (get eff :vec) :line))))))

(deftest remove-type-contributions-test
  (testing "removing type contributions clears a namespace's types"
    (let [ts (base-type-state)
          ts2 (-> ts
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map])
                  (types/register-type-contribution "ns.b" :line '#{Line} [:vec]))
          ts3 (types/remove-type-contributions ts2 "ns.a")]
      (is (nil? (get-in ts3 [:contributions "ns.a"])))
      (is (some? (get-in ts3 [:contributions "ns.b"])))
      (let [eff (types/effective-types ts3)]
        ;; ns.a's :point should be gone
        (is (nil? (get eff :point)))
        (is (not (contains? (get eff :map) :point)))
        ;; ns.b's :line should remain
        (is (= '#{Line} (get eff :line)))
        (is (contains? (get eff :vec) :line)))))

  (testing "removing a non-existent namespace is a no-op"
    (let [ts (base-type-state)
          ts2 (types/register-type-contribution ts "ns.a" :point '#{Point} [:map])
          ts3 (types/remove-type-contributions ts2 "ns.nonexistent")]
      (is (= (types/effective-types ts2) (types/effective-types ts3)))))

  (testing "base types are never affected by removal"
    (let [ts (base-type-state)
          ts2 (types/register-type-contribution ts "ns.a" :point '#{Point} [:map])
          ts3 (types/remove-type-contributions ts2 "ns.a")
          eff (types/effective-types ts3)]
      ;; :vec and :coll from base should always be present
      (is (contains? eff :vec))
      (is (contains? eff :coll))
      (is (= (get eff :vec) (get (:base ts) :vec))))))

(deftest idempotent-type-rebuild-test
  (testing "remove + re-register produces identical effective types"
    (let [ts (base-type-state)
          ts1 (types/register-type-contribution ts "ns.a" :point '#{Point} [:map])
          eff1 (types/effective-types ts1)
          ;; Simulate rebuild: remove then re-register
          ts2 (-> ts1
                  (types/remove-type-contributions "ns.a")
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map]))
          eff2 (types/effective-types ts2)]
      (is (= eff1 eff2))))

  (testing "rebuild with changed types reflects the new types"
    (let [ts (base-type-state)
          ts1 (types/register-type-contribution ts "ns.a" :point '#{Point} [:map])
          ;; Rebuild: ns.a now defines :point3d instead of :point
          ts2 (-> ts1
                  (types/remove-type-contributions "ns.a")
                  (types/register-type-contribution "ns.a" :point3d '#{Point3D} [:map]))
          eff (types/effective-types ts2)]
      ;; Old :point should be gone
      (is (nil? (get eff :point)))
      (is (not (contains? (get eff :map) :point)))
      ;; New :point3d should be present
      (is (= '#{Point3D} (get eff :point3d)))
      (is (contains? (get eff :map) :point3d))))

  (testing "no duplication on rebuild"
    (let [ts (base-type-state)
          ts1 (types/register-type-contribution ts "ns.a" :point '#{Point} [:map])
          ;; Rebuild without clean → would have double if not idempotent
          ts2 (-> ts1
                  (types/remove-type-contributions "ns.a")
                  (types/register-type-contribution "ns.a" :point '#{Point} [:map]))
          group-members (get (types/effective-types ts2) :map)]
      ;; :point should appear only once in :map's members
      (is (= 1 (count (filter #{:point} group-members)))))))
