---
title: cljs-incremental-build-problem
created: 2026-02-21
updated: 2026-02-21
type: open-thread
subsystem: cljs-compat
status: resolved
tags: [cljs, shadow-cljs, incremental-builds, state-model, cache]
related: ["[[state-bookkeeping-needs]]", "[[poly-compiler-as-value]]", "[[state-shape-aligned-with-poly-compiler]]"]
---

# CLJS Incremental Build Problem

## The Problem

Nemesis compile-time state is built up as macros expand during namespace compilation. On CLJS (shadow-cljs), this creates a conflict between correctness and build speed.

### Current Approach

```
shadow-cljs.edn:
  :build-hooks [(nemesis.core/reset-state!)]  ;; nuke state before each build
  :build-options {:cache-level :off}           ;; recompile everything every time
  :cache-blockers #{nemesis.core}              ;; never cache nemesis itself
```

`reset-state!` clears the `state` atom, the `prototypes` atom, and re-initializes the type registry. Then shadow recompiles ALL namespaces, re-expanding all `defg`/`generic+`/`fork` macros which rebuild the state from scratch.

**This works but defeats incremental compilation entirely.** Every save triggers a full rebuild.

### Why Incremental Builds Break

1. `reset-state!` nukes all compile-time state
2. Shadow only recompiles changed files (+ direct dependents)
3. Unchanged files don't re-expand macros → their `defg`/`generic+` registrations are lost
4. The compile-time state is now incomplete → extensions reference specs that don't exist → compilation fails

## Why It's Hard

### Non-idempotent mutations

`extend-spec!` concatenates cases onto existing specs. Re-running an extension doubles the cases. You can't just "replay" — you need the right starting state.

```clojure
;; First run:  spec.cases = [original-cases... extension-cases...]
;; Re-run:     spec.cases = [original-cases... extension-cases... extension-cases...]  ← WRONG
```

### Cross-namespace entanglement

Specs reference each other:
- `fork` clones a spec from another namespace (`:cloned-from` field)
- `generic+` mutates a spec defined in another namespace
- `type+` extends multiple generics at once
- `deft` does `register-type` which triggers `extend-class` re-extension

### Ordering matters

If ns B extends a generic from ns A, A must be compiled first. The spec evolves through mutations in namespace-loading order.

## Possible Strategies

### A: Snapshot + Invalidate
Store compiler state snapshots at namespace boundaries. On incremental build, restore pre-change snapshot and recompile from there. Needs transitive invalidation graph.

### B: Mutation Ledger (the `:namespaces` key)
Record every state mutation with namespace provenance. On rebuild, filter out mutations from changed namespaces, replay the rest, then recompile changed ones. Event-sourced approach — complex but precise.

### C: Separation of concerns
**Split defg into "declare" (compile-time spec) and "emit" (code generation).** If specs are pure data that can be serialized, store them per-namespace. On incremental build, restore specs from unchanged namespaces, then recompile changed ones. This aligns with the poly compiler-as-value pattern.

### D: Accept full rebuild, optimize speed
Keep `:cache-level :off` but make compilation faster. The poly layer's pure functions should be quick. The bottleneck is macro expansion + code emission — can this be parallelized?

## Architectural Insight

The prototype atom architecture (where protocol impls read through `(get-in @prototypes ...)` at runtime) is actually CLJS-friendly — it means the emitted JS code is self-consistent regardless of compilation order. The problem is purely in the compile-time macro expansion state.

## Resolution

Implemented **namespace-keyed spec registration** — a variant of Strategy C. See [[namespace-keyed-spec-registration]] for the full design.

The key insight: instead of making every mutation a pure `assoc`, we store extension cases keyed by source namespace. This makes `remove-ns-contributions` + re-expansion idempotent. A `prepare-ns!` guard in each macro handles cleanup automatically on first expansion per namespace per build.

### shadow-cljs Configuration for Incremental Builds

Replace `reset-state!` with `prepare-state!` and remove `:cache-level :off`:

```clojure
{:build-hooks [(nemesis.core/prepare-state!)]
 ;; no :build-options {:cache-level :off}
 :cache-blockers #{nemesis.core}}
```

## Previous Assessment

Strategy C feels most aligned with the poly refactoring direction. If the poly compiler is a serializable value, you can snapshot it after each namespace and restore on incremental builds. The key enabler: making `register-spec!` the only mutation (no `extend-spec!`), with extensions producing new complete specs rather than mutating existing ones.
