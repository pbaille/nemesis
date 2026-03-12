---
title: state-bookkeeping-needs
created: 2026-02-21
updated: 2026-02-21
type: open-thread
subsystem: extension
status: resolved
tags: [state-model, extension-modes, forking, sync, bookkeeping]
related: ["[[state-shape-aligned-with-poly-compiler]]", "[[poly-compiler-as-value]]", "[[runtime-state-facilities]]", "[[forking-semantics]]", "[[three-extension-modes]]"]
---

# State Bookkeeping Needs for Extensions, Forks, and Sync

The nemesis state originally had three placeholder keys (`:guards`, `:namespaces`, `:prototypes`) that were never wired up but were sketched as bookkeeping needed for correct extension/fork/sync behavior. These were removed as part of the `:fns` → `:functions` alignment, but the underlying design concerns remain open.

## The Three Concerns

### 1. Namespace Provenance (`:namespaces`)

**Problem**: When `register-type` or `generic+` is called, compile-time state mutates but there's no record of *which namespace* made the change. This matters for:

- **CLJS incremental builds**: `reset-state!` (shadow-cljs `:compile-prepare` hook) nukes all state. On recompile, namespace contributions need to replay in order. Currently this works because shadow-cljs re-requires all namespaces, but there's no explicit tracking.
- **Diagnostics**: "Extension mode :extend blocked" — which namespace tried to override?
- **Undo/replay**: If we ever need to unload a namespace's contributions (hot reload), we'd need to know what it added.

**Possible shape**: `{:namespaces {"my.ns" {:functions #{...} :types #{...} :extensions [...]}}}` — a ledger of what each namespace contributed.

### 2. Predicate Guards (`:guards`)

**Problem**: The type hierarchy is class-based. `:vec` maps to `IPersistentVector`. But what about predicate-based dispatch — "`:positive` means `(and (number? x) (pos? x))`"?

The `symbolic-pred` / `compile-pred-map` machinery in `nemesis.types` already compiles type predicates from the registry. Guards would extend this to user-defined predicates that don't map to classes.

**Tension**: Guards would require runtime dispatch (can't resolve `:positive` to a class at compile time), which conflicts with the protocol-based dispatch that gives nemesis its performance. Either guards dispatch differently (slower path), or they compile to a cond-chain before protocol dispatch.

### 3. Compile-time Prototype Mirror (`:prototypes`)

**Problem**: The runtime `prototypes` atom holds `{type-keyword → generic-name → arity → fn}`. During macro expansion, you sometimes need to know what implementations exist at runtime. Currently fork works without this because:

1. `clone-spec!` copies the compile-time spec
2. `declaration` emits fresh `prototypes_registering` code
3. Protocol extension emits new `extend-type` calls

But if someone extends a generic *at runtime* (not through macros), the compile-time spec wouldn't know about it. And `types-syncronisation` — which re-extends generics when a new type is registered — only covers compile-time cases.

**Question**: Is runtime-only extension a supported use case? If all mutations go through macros, the compile-time spec is always authoritative and a prototype mirror is unnecessary.

## Design Space

These could live as:
- **Fields in the poly compiler** (if they're general polymorphism concerns)
- **Nemesis-specific state alongside the compiler** (`{:clj {:compiler poly-val :namespaces {} :guards {}} ...}`)
- **Separate atoms** (like prototypes already is)
- **Derived at query time** (e.g., namespace provenance could be inferred from spec metadata)

## Current Status

**Namespace Provenance**: ✅ Resolved. Both function specs/extensions and type contributions are now keyed by source namespace. `prepare-ns!` cleans both on incremental rebuild. Backward compatibility paths removed — `extension-ns` is required everywhere.

**Predicate Guards**: Design complete — see [[predicate-guards-design]]. Two-phase dispatch (cond-chain for guards → protocol fallback). Guards registered via `defguard` with `:base` type + `:pred` function. Stored on compiler value as `:guards` map. Integration with extension modes via type hierarchy (guard registered as child of base type). Implementation pending.

**Compile-time Prototype Mirror**: ✅ Closed — unnecessary. Analysis (2026-02-22): The `prototypes` atom is never read during macro expansion. All `deref` calls in `compiler/forms.clj` are inside quoted (emitted) code — they execute at runtime, not macro-time. `fork` uses `compiler/clone-function` which operates purely on the compile-time state, then emits fresh `prototype_registering` code. There is no public API for direct `swap!` on prototypes — it is populated exclusively by macro-emitted code (`prototype_registering`, `guard_registering`). Therefore the compile-time spec is always authoritative and a prototype mirror is unnecessary. If runtime-only extension ever becomes a supported use case (see [[runtime-state-facilities]]), this decision should be revisited.
