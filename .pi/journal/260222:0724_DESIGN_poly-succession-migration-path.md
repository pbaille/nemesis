---
type: design
tags: [poly-migration, architecture, naming, succession, migration-path]
status: active
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-52-19-501Z_27173321-567b-47e8-9271-86002edc5885.jsonl
---

# Poly Succession: Migration Path from Nemesis

## Context

Two things happened this session:

1. **Completed the state-routing task** — all `nemesis.impl.registry` and `nemesis.core` state mutations now go through `poly.compiler.core` pure functions. No more raw `state/swap! assoc-in` or multi-step `state/swap!` sequences. See `260221:2050_TASK_route-state-mutations-through-poly-compiler.md`.

2. **Pierre clarified the vision** — poly is not an internal refactoring layer under nemesis. Poly *replaces* nemesis entirely, including the name. The `nemesis.*` namespaces will disappear. The library ships as `poly`.

## Key Decisions

- **Name is `poly`**, no concern about Polylith overlap.
- **Pure/impure boundary is sacred**: `poly.compiler.*`, `poly.functions.*`, `poly.types.*`, `poly.utils.*` stay pure. `poly.state` is the single impure namespace. `poly.core` macros bridge the two.
- **Global state is necessary and accepted** — macros need compile-time state between expansions. `poly.state` owns this explicitly.

## Existing Poly Scaffolding

Already partially implemented (discovered during session):

- `src/poly/state.clj` — atom + swap!/get, but missing `*expansion-state*`, `:type-state` in state0, ns preparation tracking. Also has a bug: calls `(exp/cljs?)` without passing expansion state.
- `src/poly/utils/expansion.clj` — cleaner than nemesis: takes expansion state as explicit argument (`cljs? state`, `qualify-symbol state x`) rather than reading a dynamic var.
- `src/poly/core.clj` — just `(defonce prototypes (atom {}))`, waiting for macros.

## Target Architecture

```
poly.core              — macros: defg, generic+, type+, thing, fork, deft
poly.state             — THE atom, expansion state, platform detection, ns tracking

poly.compiler.core     — pure: (compiler, args) → compiler'
poly.compiler.forms    — pure: (compiler, spec) → emitted-code
poly.compiler.data     — pure: compiler value constructors
poly.functions.*       — pure: parse, spec ops, registry ops
poly.types.*           — pure: hierarchy, contributions, effective-types
poly.utils.*           — pure: naming, expansion helpers, misc
```

Invariant: pure core never reads `poly.state`. Impurity flows only through `poly.core` macros.

## Migration Steps (ordered)

### 1. Complete `poly.state`

Port missing pieces from `nemesis.state`:
- `*expansion-state*` dynamic var (or decide on explicit threading — `poly.utils.expansion` already designed for it)
- Add `:type-state {:base {} :contributions {}}` to `state0`
- Namespace preparation tracking (`prepared-namespaces` atom, `ns-prepared?`, `mark-ns-prepared!`, `reset-prepared!`)
- Fix `cljs?` calls to pass expansion state
- `qualify-symbol` (delegate to `poly.utils.expansion`)

Self-contained, unblocks everything else.

### 2. Move macros to `poly.core`

Port `defg`, `generic+`, `fork`, `fork+`, `type+`, `thing`, `deft`, `register-type`, `implements?` from `nemesis.core`.

Key change: macros call `poly.state/swap!` with `poly.compiler.core` functions directly — no `nemesis.impl.registry` middleman. The registry layer (register-spec!, extend-spec!, clone-spec!) dissolves; its logic is just `(state/swap! compiler/add-function spec)`.

The name-resolution concern (aliased symbols like `one/foo` → `nemesis.tries.one/foo`) needs to be handled in the macro layer, same as the fix applied in this session's registry work.

### 3. Absorb `nemesis.impl.forms` into `poly.compiler.forms`

- `current-compiler` bridge becomes just "read poly.state" — no cross-system bridging
- `with-nemesis-prototypes` becomes unnecessary (prototypes atom lives in `poly.core`)
- `*prototypes-sym*` binding already exists in `poly.compiler.forms`; just needs the right default

### 4. Absorb `nemesis.types` into `poly.types`

- `classes` resolver with `:default`/`:nil` special handling → move to `poly.types.core`
- `init!` (populate base types) → becomes `poly.state` initialization or a `poly.core` setup fn
- Predicates (`get-preds`, type predicate compilation) → need a poly home

### 5. Delete `nemesis.*` (or keep as deprecation shim)

Once all functionality lives in `poly.*`, nemesis namespaces either:
- Get deleted (clean break)
- Become thin re-export shims for backward compatibility

## Open Design Question: Expansion State Threading

`nemesis.state` uses a dynamic var (`*expansion-state*`) — every macro binds it, functions read it implicitly.

`poly.utils.expansion` was designed for explicit passing: `(cljs? state)`, `(qualify-symbol state x)`.

The explicit approach is purer but requires threading through every function the macro calls. The pragmatic middle ground (current): dynamic var exists in `poly.state`, but pure compiler functions receive expansion context as part of the compiler value (already in `{:expansion {:env &env :form &form}}`). Pure modules never touch the dynamic var.

## Files Changed This Session

- `src/poly/compiler/core.clj` — added `register-type-contribution`
- `src/nemesis/impl/registry.clj` — all stateful ops now route through `compiler/*`
- `src/nemesis/core.clj` — `register-type` routes through `compiler/register-type-contribution`
- `.knowledge/notes/poly-replaces-nemesis.md` — new note capturing the succession decision
- `.knowledge/notes/poly-migration-strangler-fig.md` — updated with reframe
- `.knowledge/maps/architecture-map.md` — added target state section

## Vault Cross-References

- `.knowledge/notes/poly-replaces-nemesis.md` — the succession decision
- `.knowledge/notes/poly-migration-strangler-fig.md` — strangler fig strategy (still valid as migration method)
- `.knowledge/notes/poly-compiler-as-value.md` — compiler-as-value pattern
- `.knowledge/notes/expansion-state-keep-as-is.md` — why `*expansion-state*` stayed simple (in nemesis; may revisit for poly)
- `260221:2050_TASK_route-state-mutations-through-poly-compiler.md` — the task completed just before this discussion
