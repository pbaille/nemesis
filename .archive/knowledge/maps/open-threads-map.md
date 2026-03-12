---
title: open-threads-map
created: 2026-02-22
updated: 2026-02-22T07:30
type: decision
subsystem: meta
status: active
tags: [map, open-threads, navigation, design]
---

# Open Threads Map

Living index of unresolved design problems in poly/nemesis. Updated as threads are opened, resolved, or parked.

## Active

No active design threads — all major concerns resolved. 🎉

## Parked

Good ideas that aren't blocking anything. Worth revisiting when the core system is stable.

- [[runtime-state-facilities]] — Wrap/update/extract on the prototypes atom. Interesting for debugging and middleware, but CLJ/CLJS divergence makes it non-trivial.
- [[defg-as-defn]] — Lazy generic promotion. Functions start as plain `defn`, become protocol-backed when first extended. Enforces first-arg convention. Architecturally complex.

## Resolved

Design problems that have been settled.

- [[state-bookkeeping-needs]] — All three concerns resolved: namespace provenance (ns-keyed contributions), predicate guards (implemented), compile-time prototype mirror (closed — unnecessary, compile-time spec is authoritative).
- [[forking-semantics]] — Full structural clone, fork resets to `:override`, isolation tested across fork chains. All sub-questions (runtime state, structural sharing, fork-of-fork, mode inheritance) assessed as non-blocking or already resolved.
- [[three-extension-modes]] — All four modes implemented and tested: `:sealed` (no extensions), `:extend` (strictest, blocks specialization), `:refine` (default — allows specialization, blocks direct overrides), `:override` (no checks). Names finalized: tune→refine, patch→override. Default changed from override to refine. Fork resets to :override.
- [[case-accumulation-order]] — Resolved by namespace-keyed spec registration. Cases are never removed during accumulation; ordering is deterministic via reverse-sorted namespace keys.
- [[expansion-state-keep-as-is]] — `*expansion-state*` stays as `{:env &env :form &form}`. Not replaced with full compiler value.
- [[cljs-incremental-build-problem]] — Resolved by `prepare-state!` + namespace-keyed contributions.
- [[poly-migration-strangler-fig]] — Complete. Nemesis deleted, poly is the sole implementation.
- [[poly-replaces-nemesis]] — Decided. Clean break, no shim.

## By Subsystem

### Generics
- [[three-extension-modes]] (resolved)
- [[case-accumulation-order]] (resolved)
- [[defg-as-defn]] (parked)
- [[runtime-state-facilities]] (parked)

### Extension
- [[forking-semantics]] (resolved)
- [[three-extension-modes]] (resolved)

### Types
- [[state-bookkeeping-needs]] — all sections resolved

### Poly Architecture
- [[expansion-state-keep-as-is]] (resolved)
- [[poly-compiler-as-value]] (active — ongoing architectural pattern)
- [[state-shape-aligned-with-poly-compiler]] (resolved)

### CLJS Compatibility
- [[cljs-incremental-build-problem]] (resolved)
