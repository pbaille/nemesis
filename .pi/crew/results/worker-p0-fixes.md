# worker-p0-fixes Results

All 3 bugs fixed. Tests pass (`clj -M:test` → ALL TESTS OK).

## BUG-1: `:boolean` type — wrong class name
**File:** `src/thetis/types/data.clj` line 12
**Change:** `clojure.lang.Boolean` → `java.lang.Boolean`
`clojure.lang.Boolean` doesn't exist; the JVM Boolean class lives in `java.lang`.

## BUG-2: `isa` macro — silent nil for unknown types
**File:** `src/thetis/types.clj` (isa macro, `[t x]` arity)
**Change:** Added `:else` clause to `cond` that throws at macro-expansion time:
```clojure
:else (throw (ex-info (str "thetis.types/isa - Unknown type: " t) {:type t}))
```
The throw is in the macro body (not emitted as runtime code), so it fires during
macroexpansion when an unrecognized type keyword is used.

## BUG-3: `childof`/`parentof` — false positives for leaf types
**File:** `src/thetis/types/core.clj` lines ~70-82
**Root cause:** Both functions used `set/subset?` on children sets. When both types
are leaves (no children), `(subset? #{} #{})` is true — wrong result.

**Fix:** Use `children` (which walks all descendants) to test reachability:
```clojure
(defn childof [reg x y]
  (when (some #{x} (children reg y)) x))

(defn parentof [reg x y]
  (when (some #{y} (children reg x)) x))
```
`childof` now correctly asks "does x appear in y's descendants?"
`parentof` now correctly asks "does y appear in x's descendants?"
Two leaf types with empty children sets both return nil.
