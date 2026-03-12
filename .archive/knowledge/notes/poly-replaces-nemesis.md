---
title: poly-replaces-nemesis
created: 2026-02-21
updated: 2026-03-12
type: decision
subsystem: poly
status: resolved
tags: [architecture, migration, naming, vision]
related: ["[[poly-migration-strangler-fig]]", "[[poly-compiler-as-value]]"]
---

# Poly Replaces Nemesis — Full Succession, Not Internal Refactoring

## Decision

Poly is not an internal implementation layer under nemesis. The intent is for poly to **replace nemesis entirely**, including the name. The `nemesis.*` namespaces will go away. The library ships as `poly`.

## Implications

### Strangler fig is the migration strategy, not the end state

The strangler fig approach (nemesis delegates to poly internals) is still how we get there. But the finish line isn't "clean nemesis internals" — it's "no nemesis namespaces at all." The current layering (`nemesis.core` → `nemesis.impl.*` → `poly.*`) is temporary scaffolding.

### What needs to move to poly

Everything currently in `nemesis.*`:

| nemesis namespace | content | poly destination (TBD) |
|---|---|---|
| `nemesis.core` | Public macros: `defg`, `generic+`, `fork`, `type+`, `thing`, `deft` | `poly.core`? `poly.api`? |
| `nemesis.state` | Compile-time atom, `*expansion-state*`, platform detection | `poly.state`? |
| `nemesis.types` | Type registry init, `classes` resolver, predicates | `poly.types.core`? merge into existing? |
| `nemesis.impl.forms` | Code generation bridge (`current-compiler`, wrappers) | Absorbed into `poly.compiler.forms` |
| `nemesis.impl.registry` | Stateful wrappers over poly pure functions | Absorbed into `poly.compiler.core` or new `poly.compiler.state` |
| `nemesis.impl.parse` | Already just re-exports `poly.functions.parse` | Deleted |
| `nemesis.impl.utils` | Already mostly delegated to `poly.utils.*` | Deleted |

### The purity tension sharpens

Poly modules are currently pure — no global state. But macros *require* mutable compile-time state. When poly absorbs nemesis, the purity invariant needs refinement:

- **Pure core**: `poly.functions.*`, `poly.types.*`, `poly.compiler.{core,forms,data}` — pure functions on values
- **Stateful shell**: A new namespace (e.g., `poly.state` or `poly.compiler.state`) explicitly owns the atom and dynamic vars
- **Macro layer**: `poly.core` (or wherever macros live) is the entry point that ties state + pure core together

### Decided

1. **Name is `poly`** — no concern about Polylith naming overlap.
2. **Pure/impure boundary is preserved and explicit**:
   - Pure core: `poly.compiler.*`, `poly.functions.*`, `poly.types.*`, `poly.utils.*` — no global state, functions on values
   - Impure shell: `poly.state` — the one namespace that owns mutable compile-time state (atom, dynamic vars, ns tracking)
   - Macro layer: `poly.core` — macros that read/write `poly.state` and call pure compiler functions

3. **Global state is necessary and accepted** — macros need somewhere to stash specs between expansions. `poly.state` owns this explicitly. The invariant: pure modules never read `poly.state`; impurity flows only through `poly.core`.

### Open questions

1. **Same macro names?** — `defg`, `generic+`, etc. stay the same?
2. **Deprecation path** — Clean break or compatibility shim?
3. **Expansion state threading** — `poly.utils.expansion` already designed for explicit passing vs nemesis's dynamic var. Which approach for `poly.state`?

### Existing poly scaffolding

Already exists (partially implemented):
- `poly.state` — atom + swap!/get, but missing `*expansion-state*`, `:type-state`, ns tracking
- `poly.utils.expansion` — explicit expansion state passing (`cljs? state`, `qualify-symbol state x`)
- `poly.core` — just `prototypes` atom, waiting for macros

## Status — RESOLVED

**Done.** Nemesis namespaces deleted on 2026-02-22. Clean break, no shim.
Further rename `poly.*` → `thetis.*` on 2026-02-22. Artifact stays `pbaille/nemesis` 0.2.0.

All thetis tests pass (tries one–five, CLJ + CLJS).
shadow-cljs.edn updated to reference `thetis.core`.

The answers to the open questions:
1. **Same macro names** — yes, `defg`, `generic+`, etc. Same API, now under `thetis.core`.
2. **Deprecation path** — Clean break. No compatibility shim.
3. **Expansion state** — Dynamic var in `thetis.state`, pure modules get expansion context via the compiler value `{:expansion {:env &env :form &form}}`.

**Note:** Throughout this note, `poly.*` references refer to the intermediate namespace naming. The final namespaces are `thetis.*`.
