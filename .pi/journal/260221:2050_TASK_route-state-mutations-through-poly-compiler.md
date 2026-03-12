---
type: task
tags: [poly-migration, state-management, strangler-fig, compiler-as-value]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-39-57-828Z_35e5df21-68fa-4a89-9007-d8f00fbe1e18.jsonl
---

# Route state mutations through poly compiler API

## Context

The incremental build story is now complete: both function specs and type contributions are namespace-keyed with `prepare-ns!` cleaning both on rebuild. The nemesis state shape matches the poly compiler shape:

```clojure
;; nemesis.state atom
{:clj  {:functions {} :types {} :type-state {:base {} :contributions {}}}
 :cljs {:functions {} :types {} :type-state {:base {} :contributions {}}}}

;; poly.compiler.core compiler value
{:functions {} :types {} :type-state {:base {} :contributions {}}}
```

The strangler fig migration note (`poly-migration-strangler-fig.md`) identifies the next phase: nemesis macros still mutate state via raw `state/swap! assoc-in` and `state/swap! update` calls. These should go through `poly.compiler.core` update functions instead, so that:

1. **All state transitions are defined in pure poly code** — nemesis.state just holds the atom
2. **nemesis.impl.registry becomes a thinner wrapper** — mostly `state/swap!` + poly.compiler.core call
3. **Poly compiler API becomes the single source of truth** for what operations are valid on the state

### What was just done (previous session)

- `merge-cases` / `extend-spec` / `extend-function`: `extension-ns` changed from optional kwarg to required positional arg, legacy flat-concat path removed
- `effective-cases`: legacy fallback to raw `:cases` removed, assert added for initialized specs
- Type contributions now namespace-keyed: `poly.types.core` has `make-type-state`, `effective-types`, `register-type-contribution`, `remove-type-contributions`
- `prepare-ns!` now cleans both function and type contributions
- `shadow-cljs.edn` switched from `reset-state!` to `prepare-state!`

### Key architecture notes

- `nemesis.state/*expansion-state*` stays as `{:env &env :form &form}` (per `expansion-state-keep-as-is.md`) — it does NOT become a compiler value
- `nemesis.impl.forms/current-compiler` builds a fresh poly compiler snapshot from nemesis state on each call — this is correct and cheap
- `poly.compiler.core` already has `add-function`, `extend-function`, `clone-function`, `add-type`, `remove-type`, `update-types`, `remove-ns-contributions` — most operations already exist

## Files

### Primary targets

- `src/nemesis/impl/registry.clj` — currently has raw `state/swap! assoc-in [:functions ...]` calls alongside poly delegation. These should use `poly.compiler.core` functions applied via `state/swap!`.
- `src/nemesis/core.clj` — `register-type` macro does `state/swap! update :type-state poly.types/register-type-contribution` and `state/swap! assoc :types ...` — two separate swaps that should be one atomic operation through the compiler.
- `src/poly/compiler/core.clj` — may need new functions (e.g., `register-type-contribution` that wraps the type-state + effective-types dance into one call)

### Reference

- `src/nemesis/impl/forms.clj` — `current-compiler` builds the bridge; should be unaffected but verify
- `src/nemesis/state.clj` — the atom wrapper; interface should be unchanged but may simplify
- `src/poly/compiler/data.clj` — compiler constructor; may need `make-compiler` to accept `:type-state`
- `.knowledge/notes/state-shape-aligned-with-poly-compiler.md` — documents the alignment
- `.knowledge/notes/poly-compiler-as-value.md` — documents the compiler-as-value pattern
- `.knowledge/notes/expansion-state-keep-as-is.md` — documents why `*expansion-state*` stays simple

### Tests

- `test/poly/functions/spec_test.clj`
- `test/poly/incremental_build_test.clj`
- `test/poly/types/core_test.clj`
- `src/nemesis/tries/{one,two,three,four}.cljc`

## Task

### 1. Audit current raw state mutations

Grep for all `state/swap!` calls in `nemesis.impl.registry` and `nemesis.core` and classify each:

| Call site | Current form | Poly compiler equivalent |
|---|---|---|
| `register-spec!` | `state/swap! assoc-in [:functions fullname] spec` | `compiler/add-function` |
| `extend-spec!` | via `register-spec!` (same) | `compiler/extend-function` |
| `clone-spec!` | via `register-spec!` (same) | `compiler/clone-function` |
| `remove-ns-contributions!` | `state/swap! update :functions fn.reg/remove-ns-contributions` + type cleanup | `compiler/remove-ns-contributions` |
| `register-type` (core.clj) | Two separate `state/swap!` for `:type-state` and `:types` | Need a new `compiler/register-type-contribution` |

### 2. Add missing poly.compiler.core functions

- `register-type-contribution [compiler ns-str tag members groups]` — wraps `types/register-type-contribution` + `types/effective-types` into one atomic operation on the compiler value
- Possibly `init-types [compiler base-types]` — sets up the `:type-state` and `:types` from base types
- Update `make-compiler` in `poly.compiler.data` to optionally accept/setup `:type-state`

### 3. Replace raw mutations in nemesis.impl.registry

Change `register-spec!`, `extend-spec!`, `clone-spec!`, `remove-ns-contributions!` to use a pattern like:

```clojure
(defn register-spec! [spec]
  (state/swap! compiler/add-function spec)
  ;; Return the registered spec (now initialized)
  (get-spec! (:fullname spec)))
```

Instead of the current:

```clojure
(defn register-spec! [spec]
  (let [spec (if (:declaration-cases spec) spec (fn.spec/init-spec spec))]
    (state/swap! assoc-in [:functions (:fullname spec)] spec)
    spec))
```

The `init-spec` call should happen inside `compiler/add-function` (it already does via `fn.reg/add-spec`).

### 4. Replace raw mutations in nemesis.core/register-type

The two `state/swap!` calls should become one:

```clojure
(state/swap! compiler/register-type-contribution current-ns tag classes groups)
```

### 5. Verify nemesis.impl.forms/current-compiler still works

Since the state shape is unchanged (just the mutation path changed), `current-compiler` should be unaffected. But verify that `(state/get)` returns the right thing.

### 6. Clean up nemesis.impl.registry requires

After routing through compiler, `poly.functions.spec`, `poly.functions.registry`, and `poly.types.core` may no longer need to be direct dependencies of `nemesis.impl.registry` — they'd be transitive through `poly.compiler.core`.

### 7. Validate

- All existing tests pass: `clj -Sdeps '{:paths ["src" "test"]}' -M -e '(require (quote [clojure.test :refer [run-tests]])) (require (quote poly.functions.spec-test)) (require (quote poly.incremental-build-test)) (require (quote poly.types.core-test)) (run-tests (quote poly.functions.spec-test) (quote poly.incremental-build-test) (quote poly.types.core-test))'`
- Integration tests pass: `clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (require (quote nemesis.tries.four)) (println "ALL TESTS OK"))'`

### Invariants

- Poly modules remain pure (no global state)
- The poly compiler value is the single abstraction for state shape — nemesis.state is just the atom wrapper
- `current-compiler` continues to work as-is (reads state, builds poly-compatible snapshot)
- No user-facing API changes (macros behave identically)
- Incremental build behavior unchanged
