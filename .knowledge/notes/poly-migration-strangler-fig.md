---
title: poly-migration-strangler-fig
created: 2026-02-21
updated: 2026-02-22
type: decision
subsystem: poly
status: resolved
tags: [architecture, migration, refactoring]
related: ["[[poly-compiler-as-value]]"]
---

# Strangler Fig Migration: nemesis → poly

## Decision

Instead of rewriting nemesis from scratch (which stalled in April 2023), we're doing an incremental strangler fig migration: nemesis's macro layer stays intact, but its internals gradually delegate to poly's pure functional modules.

## What's Done

All four nemesis impl modules now delegate to poly:

| nemesis module | poly module | delegation style |
|---|---|---|
| `nemesis.impl.parse` | `poly.functions.parse` | Full — just re-exports |
| `nemesis.impl.registry` | `poly.functions.{spec,registry}` | Pure ops delegated, stateful wrappers remain |
| `nemesis.impl.forms` | `poly.compiler.forms` | Codegen delegated, stateful calls remain |
| `nemesis.impl.utils` | `poly.utils.{misc,names}` | Shared utils delegated, nemesis-specific stays |
| `nemesis.types` | `poly.types.{core,data}` | Hierarchy + data delegated, registry state + preds stay |

## Key Design Choices

1. **`*prototypes-sym*` dynamic var** — poly.compiler.forms generates code referencing a prototypes atom. Made it configurable so nemesis can bind it to `nemesis.core/prototypes` while poly can default to `poly.core/prototypes`.

2. **Class resolver as function** — `poly.functions.spec/class-extensions` accepts either a type registry map OR a resolver function. Nemesis passes `nemesis.types/classes` (which has special `:default` and `:nil` handling) rather than a raw registry.

3. **`nemesis.state` unchanged** — The mutable state layer is the last thing to migrate. It's the bridge between macro expansion and the functional core.

## What's Left

- `nemesis.core` macros still use `nemesis.state` directly
- `nemesis.types/classes` still has hardcoded `:default` handling (could move to registry data)
- `nemesis.state` itself (the dynamic var + atom pattern) is unchanged
- CLJS compatibility untouched
- No macro API in poly (poly.core is still just a prototypes atom)

## Status — RESOLVED

**Migration complete.** Nemesis deleted 2026-02-22. The strangler fig strategy worked:
1. Pure modules built under `poly.*`
2. Nemesis internals gradually delegated to poly
3. `poly.core` built with full macro layer
4. Nemesis removed — clean break

See [[poly-replaces-nemesis]].
