---
title: namespace-keyed-spec-registration
created: 2026-02-21
updated: 2026-02-21
type: decision
subsystem: generics
status: active
tags: [cljs, incremental-builds, spec-registration, idempotent]
related: ["[[cljs-incremental-build-problem]]", "[[state-shape-aligned-with-poly-compiler]]", "[[poly-compiler-as-value]]"]
---

# Namespace-Keyed Spec Registration

## Decision

Spec case storage is split into two structures:

```clojure
{:declaration-cases [case1 case2]                          ;; from defg, immutable after creation
 :extension-cases {"ns.b" [case3 case4] "ns.c" [case5]}}  ;; from generic+, keyed by source namespace
```

The effective (flat) case list is computed by `effective-cases`: extensions (sorted by namespace key, reversed for precedence) then declaration cases.

## Why

The old flat `:cases` vector was built by `concat` in `merge-cases` — non-idempotent. Re-running an extension doubled cases. This made incremental CLJS builds impossible: `reset-state!` nuked everything, but only changed files re-expanded macros.

With namespace-keyed storage:
- `register-spec!` (defg) is a simple `assoc` → idempotent
- `extend-spec!` (generic+) accumulates under a namespace key
- `remove-ns-contributions` clears a namespace's key before recompilation
- Re-expansion after cleanup produces identical state → idempotent rebuild

## Mechanism

### Build Lifecycle (Incremental)

```
1. Build starts
2. prepare-state! (build hook) resets ns-preparation tracker
3. Shadow identifies changed namespaces, recompiles them
4. First macro in each recompiling ns calls prepare-ns!
   → removes that ns's old contributions (specs it defined + extensions it made)
5. Macros re-register idempotently
6. State is consistent
```

### prepare-ns! Guard

Each macro (`defg`, `generic+`, `fork`, `register-type`) calls `prepare-ns!` which:
- On first call per ns per build: removes that ns's contributions
- On subsequent calls (same ns, same build): no-op

This is self-contained — no dependency on shadow-cljs internals for knowing which files changed.

## Precedence

Extensions are ordered by reverse-sorted namespace keys. Within a namespace, cases appear in the order `generic+` was called. This is deterministic and matches the practical require-order in most cases.

`class-extensions` reverses the effective case list then reduces with `assoc` on `[class arity]`, so the **first** case in the effective list (= latest extension) wins.

## Fork Behavior

`clone` snapshots `effective-cases` as the new spec's `declaration-cases` with empty `extension-cases`. The fork is completely isolated — changes to the original (or its extensions) don't affect the clone.

## Resolved: Type Registry

The type registry is now namespace-keyed, mirroring the function registry pattern. State shape:

```clojure
{:type-state {:base {:vec #{IPV} :coll #{:seq :vec :set :map} ...}     ;; from init!
              :contributions {"ns.a" {:tags {:my-point #{MyPoint}}      ;; tags this ns defined
                                      :groups {:map #{:my-point}}}}}   ;; group memberships
 :types {...}}                                                          ;; flat effective view (computed)
```

Pure functions in `poly.types.core`: `make-type-state`, `effective-types`, `register-type-contribution`, `remove-type-contributions`. `prepare-ns!` now cleans both function and type contributions.
