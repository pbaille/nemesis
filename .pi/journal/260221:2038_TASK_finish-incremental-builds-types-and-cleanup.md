---
type: task
tags: [incremental-builds, type-registry, cleanup, backward-compat-removal]
status: done
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-10-49-886Z_e845fd8f-6817-4947-ad33-59751d254b7b.jsonl
---

# Finish incremental build support: remove backward compat + namespace-key types

## Context

We just implemented namespace-keyed spec registration (see `.knowledge/notes/namespace-keyed-spec-registration.md`). The function registry now stores extension cases keyed by source namespace, enabling idempotent rebuilds via `prepare-ns!` → `remove-ns-contributions` → re-expansion.

Two things remain before the incremental build story is end-to-end:

### 1. Backward compatibility cruft in spec registration

`merge-cases` in `poly.functions.spec` still has a dual-path `if extension-ns` branch — when no `extension-ns` is provided, it falls back to the old non-idempotent `concat`. Similarly `effective-cases` has a `(if (and decl (some? exts))` guard that falls back to raw `:cases`. Pierre said backward compat is not needed — all callers now pass `extension-ns`. Remove the legacy paths.

### 2. Type registry is not namespace-keyed

`register-type` in `nemesis.core` still does a direct `state/swap! update :types` — no namespace provenance. If a namespace using `deft` or `register-type` gets recompiled in an incremental build, `prepare-ns!` will clean its function contributions but NOT its type contributions. This means:

- Stale type entries survive across incremental rebuilds
- Type registry can accumulate duplicates if the same namespace re-registers types
- `remove-ns-contributions` has no type counterpart

The type registry needs the same pattern: track which namespace contributed which types/group-memberships, and clean them on `prepare-ns!`.

## Files

### Core files to modify

- `src/poly/functions/spec.clj` — remove legacy concat path from `merge-cases`, remove fallback in `effective-cases`
- `src/poly/functions/registry.clj` — remove legacy path from `extend-spec`, update `remove-ns-contributions` to also handle types
- `src/poly/compiler/core.clj` — update `extend-function` signature (extension-ns becomes required)
- `src/nemesis/impl/registry.clj` — remove legacy path from `extend-spec!`
- `src/nemesis/core.clj` — rework `register-type` macro to use namespace-keyed type storage
- `src/nemesis/state.clj` — may need type contribution tracking alongside `prepared-namespaces`
- `src/nemesis/types.clj` — `init!` and `get-reg` may need updates for the new type storage model
- `src/poly/types/core.clj` — may need a `remove-type-contributions` function

### Tests

- `test/poly/functions/spec_test.clj` — update tests that relied on legacy path
- `test/poly/incremental_build_test.clj` — add type registry incremental build tests
- `src/nemesis/tries/{one,two,three,four}.cljc` — must continue to pass

### Reference

- `.knowledge/notes/namespace-keyed-spec-registration.md` — the design note for what's already done
- `.knowledge/notes/state-bookkeeping-needs.md` — discusses the namespace provenance concern
- `shadow-cljs.edn` — current shadow config (`:cache-level :off`, `reset-state!`)

## Task

### Part 1: Remove backward compatibility

1. In `poly.functions.spec/merge-cases` — remove the `(if extension-ns ...)` branch; always use namespace-keyed storage. Make `extension-ns` a required parameter.

2. In `poly.functions.spec/effective-cases` — remove the `(if (and decl (some? exts))` fallback. Always compute from structured storage.

3. Propagate: `poly.functions.registry/extend-spec`, `poly.compiler.core/extend-function`, `nemesis.impl.registry/extend-spec!` — make `extension-ns` required (not optional keyword arg).

4. Verify no callers use the old path.

### Part 2: Namespace-key the type registry

Design the type contribution tracking. Current type state shape:

```clojure
;; nemesis.state → {:clj {:types {:vec #{IPersistentVector} :coll #{:seq :vec :set :map} ...} :functions {...}} ...}
```

`register-type` does two things:
1. Adds `tag → #{classes}` entry
2. Adds `tag` to parent group entries (e.g., `:map` group gets the new tag)

Both need provenance tracking. Proposed shape:

```clojure
{:types {:vec #{IPersistentVector} :coll #{:seq :vec :set :map} ...}           ;; effective (computed)
 :type-contributions {"ns.a" {:tags {:my-point #{MyPoint}}                     ;; tags this ns defined
                               :group-memberships {:map #{:my-point}}}}}        ;; groups this ns added to
```

Or alternatively, mirror the functions pattern more closely:

```clojure
{:types {:base {:vec #{IPV} :coll #{:seq :vec :set :map} ...}     ;; from init!
         :contributions {"ns.a" {:tags {:my-point #{MyPoint}}
                                  :groups {:map #{:my-point}}}}}}
```

Key operations needed:
- `register-type-contribution!` — idempotent, keyed by namespace
- `remove-type-contributions` — clean a namespace's type additions
- `effective-types` — compute the merged type registry from base + contributions

Consider where this lives — types are shared infrastructure, so probably `poly.types.core` for the pure functions and `nemesis.types` or `nemesis.state` for the stateful wrapper.

### Part 3: Wire into prepare-ns!

Update `nemesis.impl.registry/prepare-ns!` (or add to it) to also call the type cleanup when a namespace is being re-prepared.

### Part 4: Validate

- All existing tests pass: `clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (require (quote nemesis.tries.four)) (println "ALL TESTS OK"))'`
- Poly unit tests pass: `clj -Sdeps '{:paths ["src" "test"]}' -M -e '(require (quote [clojure.test :refer [run-tests]])) (require (quote poly.functions.spec-test)) (require (quote poly.incremental-build-test)) (run-tests (quote poly.functions.spec-test) (quote poly.incremental-build-test))'`
- Add new tests for type registry incremental build scenarios (register-type in ns-a, rebuild ns-a, verify types are correct)

### Part 5: Update shadow-cljs.edn

Once everything works, update `shadow-cljs.edn` to use the new incremental build hook:

```clojure
{:build-hooks [(nemesis.core/prepare-state!)]
 ;; remove :build-options {:cache-level :off}
 :cache-blockers #{nemesis.core}}
```

### Invariants
- Poly modules remain pure (no global state)
- `deft` and `register-type` work identically from user perspective
- Base types (`:vec`, `:map`, `:coll`, etc. from `poly.types.data`) are not affected by namespace cleanup
- Fork isolation preserved
- Case precedence unchanged
