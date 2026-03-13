# Dead Code Report — thetis

**Investigator:** dead-code-detective  
**Date:** 2026-03-13  
**Scope:** All `src/thetis/` source files (excluding `tries/`)

---

## Summary

Found **24 dead code items** across 8 files. Most significant: 3 test files are completely orphaned (wrong namespaces + not on classpath). Several utility/public API vars are defined but never used.

---

## Findings

### 🔴 HIGH CONFIDENCE — Definitely Dead

---

#### 1. `test/poly/` — All 3 test files are orphaned
**Files:** `test/poly/incremental_build_test.clj`, `test/poly/types/core_test.clj`, `test/poly/functions/spec_test.clj`

**Evidence:**
- All 3 files declare `(ns poly.*)` namespaces and require `poly.*` dependencies — the old name before the rename to `thetis`.
- No `poly.*` source files exist anywhere in `src/`.
- The `test/` directory is **not** in `:paths` in `deps.edn` (only `"src"` is listed).
- The `:test` alias in `deps.edn` only runs `thetis.tries.*`, not these files.
- These tests cannot compile or run. They are stranded artifacts from the `poly` → `thetis` rename.

**Confidence:** 100%

---

#### 2. `state.clj` — `debug` atom
```clojure
(defonce debug (atom nil))
```
**Evidence:** `grep -rn "debug" src/` — zero references outside its own definition line. Never read, never written.

**Confidence:** 100%

---

#### 3. `state.clj` — `lambda-case-compiler*`
```clojure
(def lambda-case-compiler* (atom identity))
```
**Evidence:** `grep -rn "lambda-case-compiler\*" src/` — zero references outside its own definition. The comment says "Used in 'with-compiled-cases if no overrides given" but no such var/macro exists. Likely a remnant from an earlier design.

**Confidence:** 100%

---

#### 4. `state.clj` — `get-in`
```clojure
(defn get-in [p]
  (c/get-in (current) p))
```
**Evidence:** `grep -rn "state/get-in\|state\.get-in" src/` — zero results. Never called anywhere.

**Confidence:** 100%

---

#### 5. `state.clj` — `display` macro
```clojure
(defmacro display []
  (list 'quote @state))
```
**Evidence:** `grep -rn "state/display\|state\.display" src/` — zero results. REPL debug helper, never used.

**Confidence:** 100%

---

#### 6. `state.clj` — `targeting-cljs` macro
```clojure
(defmacro targeting-cljs
  "Force CLJS compilation target (for cross-compilation)."
  [& xs]
  `(binding [*expansion-state* {:env {:ns true}}]
     ~@xs))
```
**Evidence:** `grep -rn "targeting-cljs" src/` — only its own definition. Never used externally.

**Confidence:** 100%

---

#### 7. `compiler/data.clj` — `clj-compiler` and `cljs-compiler`
```clojure
(def clj-compiler
  "A compiler initialized with Clojure base types and groups."
  (make-compiler (merge type-data/clj-base-types type-data/groups)))

(def cljs-compiler
  "A compiler initialized with ClojureScript base types and groups."
  (make-compiler (merge type-data/cljs-base-types type-data/groups)))
```
**Evidence:** `grep -rn "clj-compiler\|cljs-compiler" src/ test/` — only their definitions in `compiler/data.clj`. These pre-built compiler values are never consumed. The live code path builds compilers through `state.clj` → `thetis.state/init-types!`.

**Confidence:** 100%

---

#### 8. `compiler/core.clj` — `implementers-map`
```clojure
(defn implementers-map
  "Map of generic-name → set of implementing types."
  [compiler]
  (fn.reg/implementers-map (:functions compiler)))
```
**Evidence:** `grep -rn "implementers-map" src/ test/` — only defined in `compiler/core.clj` and `functions/registry.clj`. Never called from `core.clj`, `forms.clj`, or any macro. The `functions/spec/implementers` helper it delegates to is also only used by `implementers-map`.

**Confidence:** 100%

---

#### 9. `compiler/core.clj` — `remove-type`
```clojure
(defn remove-type
  "Remove a type from the type registry."
  [compiler tag]
  (update compiler :types types/remove-type tag))
```
**Evidence:** `grep -rn "compiler/remove-type\|compiler\.remove-type" src/ test/` — zero calls. `types/remove-type` (the pure fn) is tested in orphaned `test/poly/types/core_test.clj` but the compiler wrapper is never used.

**Confidence:** 100%

---

#### 10. `compiler/core.clj` — `update-types`
```clojure
(defn update-types
  "Apply an arbitrary function to the type registry.
   For cases where add-type/remove-type aren't sufficient..."
  [compiler f & args]
  (apply update compiler :types f args))
```
**Evidence:** `grep -rn "update-types" src/ test/` — only its definition. Never called.

**Confidence:** 100%

---

#### 11. `compiler/forms.clj` — `conj-type`
```clojure
(defn conj-type
  "Add a type to a registry with its children and parent group memberships."
  [reg {:keys [tag childs parents]}]
  ...)
```
**Evidence:** `grep -rn "conj-type" src/ test/` — only its definition at line 310. Never called anywhere. Sits in the `:type-extension` do block alongside the used `deft_impl-bind-fields`, likely a leftover from an earlier approach to type registration.

**Confidence:** 100%

---

#### 12. `compiler/forms.clj` — local `cljs_prototype-assoc-form` in `letfn` (shadowed, unused)
```clojure
(letfn [(cljs_prototype-assoc-form [obj meth impl]   ;; ← DEAD
           (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))
        ...]
  (defn cljs-extend1 [class protocol method arity impl]
    ...
    `(do ~(u/cljs_prototype-assoc-form ...)    ;; ← uses u/ version
         ~(u/cljs_prototype-assoc-form ...))))
```
**Evidence:** The `letfn` defines a local `cljs_prototype-assoc-form` but `cljs-extend1` calls `u/cljs_prototype-assoc-form` (from `utils/misc`). The local definition is never invoked. The same function exists in `utils/misc.clj` and is used from there. The local one is a shadowed dead duplicate.

**Confidence:** 99%

---

#### 13. `types/core.clj` — `make-guard-state`
```clojure
(defn make-guard-state
  "Create a guard state."
  []
  {:base {}
   :contributions {}})
```
**Evidence:** `grep -rn "make-guard-state" src/ test/` — only its definition. Guard state is initialized inline in `state.clj`'s `state0` map as `{:base {} :contributions {}}`, not via this constructor.

**Confidence:** 100%

---

### 🟡 MEDIUM CONFIDENCE — Probably Dead (API surface with no known callers)

---

#### 14. `types.clj` — `compile-pred-map` / `predmap` / `builtin-preds` cluster
```clojure
(defn compile-pred-map ...)
(defn predmap ...)
(def builtin-preds (predmap))
```
**Evidence:** `grep -rn "compile-pred-map\|predmap\|builtin-preds" src/ test/` — `compile-pred-map` is only used by `predmap`; `predmap` is only used to compute `builtin-preds`; `builtin-preds` is never referenced anywhere. The whole chain is a dead-end. Likely intended as a public API hook that nobody ended up using.

**Confidence:** 95%

---

#### 15. `types.clj` — `get-type-state`
```clojure
(defn get-type-state []
  (state/get :type-state))
```
**Evidence:** `grep -rn "get-type-state" src/ test/` — only its definition. Type state is accessed internally by `state.clj` directly.

**Confidence:** 95%

---

#### 16. `types.clj` — `cyclic?`, `all-paths`, `all-types` re-exports
```clojure
(defn all-paths ...)
(defn cyclic? [x] (types/cyclic? x))
(defn all-types ...)
```
**Evidence:** `grep -rn "types/cyclic?\|types/all-paths\|types/all-types" src/ test/` — zero external callers. These delegate to `types/core` equivalents that are used internally, but the re-exports in `types.clj` are never consumed.

**Confidence:** 90%

---

#### 17. `types.clj` — `childs`, `childof`, `parentof` re-exports
```clojure
(defn childs ...)
(defn childof ...)
(defn parentof ...)
```
**Evidence:** `grep -rn "types/childs\|childs\|childof\|parentof" src/ test/` — only `conj-type` in `forms.clj` references `:childs` as a destructured key (not a function call). These three re-exported functions have no callers. The underlying `types/core` functions (`children`, `childof`, `parentof`) are called directly by internal code.

**Confidence:** 90%

---

#### 18. `utils/misc.clj` — `pp` and `pretty-str`
```clojure
(defn pp [& xs] (mapv pprint/pprint xs) (last xs))
(defn pretty-str [& xs] (with-out-str (apply pp xs)))
```
**Evidence:** `grep -rn "u/pp\b\|pretty-str" src/ test/` — `pp` only appears in `pretty-str`'s definition; `pretty-str` has zero references. Pure debug printing utilities never actually used.

**Confidence:** 95%

---

### 🟢 LOW CONFIDENCE — Suspicious but Possibly Intentional

---

#### 19. `compiler/forms.clj` — `*generic+-sym*`, `*prototypes-sym*`, `*guard-impls-sym*` dynamic vars
These three `^:dynamic` vars are documented as "rebindable for... scenarios" but are never rebound anywhere in the codebase. They function as constants. Their rebindability may be intentional public API for advanced users.

**Confidence:** 40% (likely intentional extension points)

---

#### 20. `state.clj` — `clj-state` / `cljs-state`
```clojure
(defn clj-state [] (:clj @state))
(defn cljs-state [] (:cljs @state))
```
Both are only used by `current []` internally. Could be dead or intended for REPL introspection.

**Confidence:** 60%

---

## Redundancy: Duplicate Implementation

#### `cljs_prototype-assoc-form` defined in two places
- `utils/misc.clj` line 16: `(defn cljs_prototype-assoc-form ...)`
- `compiler/forms.clj` line 94: identical `letfn` local version (dead — see finding #12)

The `misc.clj` version is the live one (called via `u/cljs_prototype-assoc-form`). The forms.clj local version is a dead duplicate.

---

## Orphaned Rename Artifacts

The `poly` → `thetis` rename (2026-02-22) left behind:

| Artifact | Location | Status |
|----------|----------|--------|
| `poly.incremental-build-test` | `test/poly/incremental_build_test.clj` | ❌ Broken namespace, not on classpath |
| `poly.types.core-test` | `test/poly/types/core_test.clj` | ❌ Broken namespace, not on classpath |
| `poly.functions.spec-test` | `test/poly/functions/spec_test.clj` | ❌ Broken namespace, not on classpath |

All require `poly.*` namespaces that don't exist. The `test/` directory is not in `:paths` in `deps.edn`. These tests have been silently broken since the rename.

---

## Quick Reference

| File | Dead Item | Type | Confidence |
|------|-----------|------|------------|
| `test/poly/*.clj` (3 files) | Entire files | Orphaned rename artifacts | 100% |
| `state.clj` | `debug` | Unused atom | 100% |
| `state.clj` | `lambda-case-compiler*` | Unused atom | 100% |
| `state.clj` | `get-in` | Unused function | 100% |
| `state.clj` | `display` | Unused macro | 100% |
| `state.clj` | `targeting-cljs` | Unused macro | 100% |
| `compiler/data.clj` | `clj-compiler` | Unused def | 100% |
| `compiler/data.clj` | `cljs-compiler` | Unused def | 100% |
| `compiler/core.clj` | `implementers-map` | Unused function | 100% |
| `compiler/core.clj` | `remove-type` | Unused function | 100% |
| `compiler/core.clj` | `update-types` | Unused function | 100% |
| `compiler/forms.clj` | `conj-type` | Unused function | 100% |
| `compiler/forms.clj` | local `cljs_prototype-assoc-form` | Dead duplicate | 99% |
| `types/core.clj` | `make-guard-state` | Unused function | 100% |
| `types.clj` | `compile-pred-map` + `predmap` + `builtin-preds` | Unused cluster | 95% |
| `types.clj` | `get-type-state` | Unused function | 95% |
| `types.clj` | `cyclic?`, `all-paths`, `all-types` | Unused re-exports | 90% |
| `types.clj` | `childs`, `childof`, `parentof` | Unused re-exports | 90% |
| `utils/misc.clj` | `pp`, `pretty-str` | Unused debug utils | 95% |
