# Bug Hunter Report — thetis

**Scope:** All source files in `src/thetis/` (excluding `tries/`)  
**Platform:** CLJ + CLJS divergence analysis included  
**Method:** Full read of all ~2500 LOC, REPL confirmation of each finding

---

## BUG-1 — `clojure.lang.Boolean` doesn't exist [HARD CRASH]

**File:** `src/thetis/types/data.clj:12`

```clojure
(def clj-base-types
  '{...
    :boolean #{clojure.lang.Boolean}   ;; ← THIS CLASS DOES NOT EXIST
    ...})
```

**What's wrong:** `clojure.lang.Boolean` is not a real Java class. Boolean values in Clojure are `java.lang.Boolean`. Confirmed:

```
$ clojure -e "(Class/forName \"clojure.lang.Boolean\")"
DOES NOT EXIST: clojure.lang.Boolean
```

**Impact:** Any generic that dispatches on `:boolean` in CLJ throws `ClassNotFoundException` at runtime. Confirmed:

```
$ clojure -e "(require '[thetis.core :as c]) (c/defg my-fn [x] :boolean \"yes\")"
Execution error (ClassNotFoundException): clojure.lang.Boolean
```

The `:boolean` type is in `clj-base-types`, registered into state at startup via `init-types!`, and used whenever the compiler emits `(import 'clojure.lang.Boolean)` + `(extend clojure.lang.Boolean ...)`. All those calls explode.

**Fix:** Change to `java.lang.Boolean`.

---

## BUG-2 — `isa` macro silently returns `nil` for unknown types [SILENT FAILURE]

**File:** `src/thetis/types.clj:93-100`

```clojure
(defmacro isa
  ([t] `(fn [x#] (isa ~t x#)))
  ([t x]
   (cond (get-type t) `(~(symbolic-pred t) ~x)
         (set? t) `(or ~@(map (fn [t] (macroexpand (list `isa t x))) t)))))
         ;; NO else clause — falls off the end → nil
```

**What's wrong:** The `cond` has no fallback. If `t` is neither a registered type keyword nor a set literal, the macro returns `nil`. The 1-arity form returns `(fn [x#] nil)` — a function that always returns nil. The 2-arity form returns `nil` which inlines as nothing.

**Confirmed:**

```
$ clojure -e "(macroexpand '(thetis.types/isa :not-a-real-type x))"
nil
```

A typo in a type keyword — `:bool` instead of `:boolean` — silently becomes a predicate that always returns nil instead of throwing at macro-expansion time. This is especially insidious with the 1-arity form since `(isa :typo)` produces `(fn [x#] nil)` — a valid function that always "fails" with no error trace.

**Fix:** Add an `else` clause that throws:
```clojure
:else (throw (ex-info (str "Unknown type: " t) {:type t}))
```

---

## BUG-3 — `childof`/`parentof` false positive for unrelated leaf nodes [WRONG SEMANTICS]

**File:** `src/thetis/types/core.clj:70-82`

```clojure
(defn childof
  "is x a child of y?"
  [reg x y]
  (when (set/subset? (set (children reg x)) (set (children reg y)))
    x))

(defn parentof
  "is x a parent of y?"
  [reg x y]
  (when (set/subset? (set (children reg y)) (set (children reg x)))
    x))
```

**What's wrong:** Both use `set/subset?` on the children sets. Two types with *no children* in the registry (leaf nodes — concrete class symbols, or user-defined types with no subtypes) always have `children = []`. An empty set is a subset of every set, including another empty set:

```
(set/subset? #{} #{}) → true
```

So `(childof reg 'MyClass1 'MyClass2)` returns `MyClass1` even when the two classes are completely unrelated.

**Confirmed:**

```
$ clojure -e "
(require '[thetis.types.core :as tc])
(def reg {:coll #{:vec :map} :vec #{java.lang.Object} :map #{java.util.Map}})
(println (tc/childof reg 'java.lang.Object 'java.util.Map))
;; => java.lang.Object  (WRONG — they're unrelated)"
```

Also confirmed with user-defined types:
```clojure
(tc/childof reg 'MyClass1 'MyClass2)  ;; → MyClass1  (should be nil)
(tc/parentof reg 'MyClass1 'MyClass2) ;; → MyClass1  (should be nil)
```

These are user-facing API functions. User code calling `(thetis.types/childof :my-type :other-type)` with two leaf types will get a false truthy result.

**Fix:** `childof` should check whether `x` appears in `(children reg y)`, not compare children sets:
```clojure
(defn childof [reg x y]
  (when (contains? (set (children reg y)) x) x))
```

---

## BUG-4 — Extension precedence is alphabetical, not temporal [WRONG DOCS / SURPRISING BEHAVIOR]

**File:** `src/thetis/functions/spec.clj:12-24`

```clojure
(defn effective-cases [spec]
  ...
  (let [sorted-ext-keys (sort (keys exts))
        ext-cases (mapcat #(get exts %) (reverse sorted-ext-keys))]
    ;;                                    ↑ reverse-alphabetical = higher keys win
    (vec (concat ext-cases decl))))
```

**What's wrong:** The docstring says "latest extensions first" but the ordering is `reverse` of `sort` on string namespace keys — i.e., **alphabetically descending**. In `:override` mode with two competing implementations of the same type:

```
z.my-module wins over a.my-module regardless of which was registered last
```

**Confirmed:**

```clojure
(let [s {:extension-cases {"z.module" [{:type :vec :tag :z}]
                           "a.module" [{:type :vec :tag :a}]}}]
  (first (spec/effective-cases s)))
;; → {:type :vec :tag :z}   ;; z.module always wins
```

If a user registers extensions in source order `a.module` → then `z.module` → then `b.module`, the dispatch order is `z > b > a`, NOT the loading order. Users expecting "last registered wins" semantics will get wrong dispatch silently in `:override` mode.

The "latest" comment in the source is factually wrong — it's lexicographic, not temporal.

---

## BUG-5 — `parentof` returns `x` but should return `y`, making it asymmetric with `childof` [API CONTRACT BUG]

**File:** `src/thetis/types/core.clj:76-79`

```clojure
(defn parentof
  "is x a parent of y?"
  [reg x y]
  (when (set/subset? (set (children reg y)) (set (children reg x)))
    x))  ;; ← returns x
```

`childof` returns `x` (the potential child), which is sensible: "yes, x is a child." But `parentof` also returns `x`. The docstring says "is x a parent of y?" — if true, returning `x` (the parent) is OK for the primary question. But compare usage:

```clojure
(childof reg :vec :coll)   ;; → :vec  (":vec is a child of :coll")
(parentof reg :coll :vec)  ;; → :coll (":coll is a parent of :vec")
```

These work. But `parentof` is also broken by the same empty-children bug (BUG-3):

```clojure
(parentof reg 'Unrelated1 'Unrelated2)  ;; → Unrelated1  (WRONG)
```

And because `children reg y` = `[]` (for leaf y), `#{} ⊆ (children reg x)` = `true` for ANY `x`. This means every type is incorrectly reported as a parent of every leaf type.

---

## Summary Table

| # | File | Line | Severity | Description |
|---|------|------|----------|-------------|
| 1 | `types/data.clj` | 12 | **CRASH** | `clojure.lang.Boolean` doesn't exist → `ClassNotFoundException` on any `:boolean` generic |
| 2 | `types.clj` | ~95 | **HIGH** | `isa` macro returns `nil` for unknown type keywords — silent predicate that always returns nil |
| 3 | `types/core.clj` | 70–79 | **HIGH** | `childof`/`parentof` false positives: any two types with no children are wrongly considered child/parent of each other |
| 4 | `functions/spec.clj` | 12–24 | **MEDIUM** | Extension precedence is alphabetical by ns name, not temporal; docstring says "latest wins" which is wrong |
| 5 | `types/core.clj` | 76–79 | **MEDIUM** | `parentof` asymmetric bug — same root cause as BUG-3, every type is a "parent" of leaf types |

---

## Non-Issues Investigated

- `CLJS core.cljs` being nearly empty: intentional — macros are CLJ-only, runtime atoms are defined in both
- `defmac` `postwalk-replace` substituting `&env`/`&form`: intentional mechanism, not a bug
- `(extend nil ...)` for `:nil` type dispatch: valid Clojure, handled correctly
- `MapEntry` appearing in both `:vec` and `:map-entry` in CLJS: the `class-extensions` reducer uses `[class arity]` as key, so the last definition wins; worth auditing but not an immediate crash
- `prepare-ns!` TOCTOU race: safe under single-threaded macro expansion (CLJ's normal case)
- Guard dispatch with variadic generics: both guard and protocol paths receive rest-args consistently
