---
title: architecture-map
created: 2026-02-21
updated: 2026-03-12
type: decision
subsystem: meta
status: active
tags: [map, architecture, navigation]
---

# Architecture Map

## Current State (thetis — sole implementation, published as `pbaille/nemesis` 0.2.0)

```
User API
  thetis.core              — macros: defg, generic+, type+, thing, fork, deft
  thetis.core.cljs         — ClojureScript stub
  thetis.types             — user-facing type queries + predicate compilation

Stateful Shell
  thetis.state             — THE atom, expansion state, platform detection,
                             ns tracking, type initialization

Pure Core
  thetis.compiler.core     — (compiler, args) → compiler'
  thetis.compiler.forms    — (compiler, spec) → emitted-code
  thetis.compiler.data     — compiler value constructors
  thetis.functions.parse   — parse defg/generic+ forms into specs
  thetis.functions.spec    — spec operations (merge, clone, class-extensions)
  thetis.functions.registry — registry operations on explicit maps
  thetis.types.core        — hierarchy queries, contributions, effective-types
  thetis.types.data        — type definitions (CLJ + CLJS base types, groups)
  thetis.utils.misc        — shared utilities + defmac
  thetis.utils.names       — naming conventions (name_derive, name_arify)
  thetis.utils.expansion   — macro expansion state utilities
```

## Dependency Flow

```
thetis.core → thetis.state ←→ thetis.compiler.core
                ↓                  ↓
           thetis.compiler.forms ← thetis.functions.{parse,spec,registry}
                ↓                  ↓
           thetis.types.{core,data}
                ↓
           thetis.utils.{misc,names,expansion}
```

## State Shape

```clojure
;; thetis.state/state atom
{:clj  {:functions {} :types {} :type-state {:base {} :contributions {}}}
 :cljs {:functions {} :types {} :type-state {:base {} :contributions {}}}}

;; compiler value (read from one platform slice + expansion context)
{:functions {} :types {} :type-state {...} :expansion {:env &env :form &form}}
```

`current-compiler` reads the platform slice and adds expansion context.

## Architecture — ACHIEVED

All integration tests pass (thetis.tries.{one,two,three,four,five}).
Invariant: pure core never reads thetis.state. Impurity flows only through thetis.core macros.

## Naming History

- Original: `nemesis.*` — protocol-based polymorphism library
- Refactored internals: `poly.*` — compiler-as-value rewrite
- Final rename: `thetis.*` (2026-02-22) — clean namespace, artifact stays `pbaille/nemesis`

## Key Notes

- [[poly-replaces-nemesis]] — the poly architecture replaced nemesis internals
- [[poly-migration-strangler-fig]] — the migration strategy (strangler fig)
- [[poly-compiler-as-value]] — the compiler-as-value pattern
- [[state-shape-aligned-with-poly-compiler]] — state/compiler shape unification

## Open Design Threads

See [[open-threads-map]] for the full index of unresolved design problems.
