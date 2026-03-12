---
type: task
tags: [poly-migration, strangler-fig, refactoring, architecture]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T12-19-34-521Z_7677bfb5-312d-43fc-a6f0-ae5c0ea94da2.jsonl
---

# Continue poly-migration: next incremental steps

## Context

We're on branch `poly-migration`, doing a strangler fig migration of nemesis internals to delegate to poly's pure functional layer. The migration is well underway — all 5 nemesis impl modules now delegate to poly, and all tests pass at every step.

### What's been done

| nemesis module | poly target | status |
|---|---|---|
| `nemesis.impl.parse` | `poly.functions.parse` | Full delegation (thin re-export) |
| `nemesis.impl.utils` | `poly.utils.{misc,names}` | Shared utils delegated, nemesis-specific retained |
| `nemesis.impl.registry` | `poly.functions.{spec,registry}` | Pure ops delegated, stateful wrappers remain |
| `nemesis.impl.forms` | `poly.compiler.forms` | Codegen delegated, 3 stateful fns remain |
| `nemesis.types` | `poly.types.{core,data}` | Data + hierarchy delegated, state + preds remain |

### Key design choices made during migration

1. **`*prototypes-sym*` dynamic var** in `poly.compiler.forms` — poly generates code referencing a prototypes atom. Made configurable so nemesis binds it to `nemesis.core/prototypes` via `with-nemesis-prototypes` macro.

2. **Class resolver as function** — `poly.functions.spec/class-extensions` accepts either a type registry map OR a `(fn [type] -> [classes...])`. Nemesis passes `nemesis.types/classes` because it has special `:default` and `:nil` handling not in the registry.

3. **`nemesis.impl.forms/current-compiler`** builds a poly compiler value from nemesis mutable state:
   ```clojure
   {:functions (reg/get-reg) :types (t/get-reg)
    :expansion {:env (state/env) :form (state/form)}
    :class-resolver t/classes}
   ```

### Remaining stateful functions in nemesis.impl.forms
- `extend-class` — reads `reg/get-class-cases`
- `protocol-extension` — reads `reg/class-extensions`
- `thing_parse-impl-cases` — reads `reg/get-spec!`
- `types-syncronisation` — reads `reg/implementers-map` + `reg/get-spec!`

## Files

Key files modified/created during migration:
- `src/nemesis/impl/parse.clj` — now thin re-export layer
- `src/nemesis/impl/utils.clj` — delegates shared utils to poly
- `src/nemesis/impl/registry.clj` — pure ops delegated to poly
- `src/nemesis/impl/forms.clj` — codegen delegated, stateful wrappers remain
- `src/nemesis/types.clj` — hierarchy + data delegated to poly
- `src/poly/compiler/forms.clj` — added `*prototypes-sym*` dynamic var
- `src/poly/functions/spec.clj` — class-extensions accepts fn resolver
- `src/poly/utils/names.clj` — populated with name_derive, name_arify
- `src/poly/utils/misc.clj` — added cljs_prototype-assoc-form
- `src/poly/types/core.clj` — fixed requires, added :refer-clojure exclude
- `src/poly/functions/parse.clj` — fixed requires for names
- `.knowledge/notes/poly-migration-strangler-fig.md` — migration decision note
- `.knowledge/notes/poly-compiler-as-value.md` — compiler pattern note
- `.knowledge/maps/architecture-map.md` — current architecture

Test files (must pass after every change):
- `src/nemesis/tries/one.cljc`
- `src/nemesis/tries/two.cljc`
- `src/nemesis/tries/three.cljc`

Run tests: `clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (println "ALL TESTS OK"))'`

## Task

Continue the poly migration with these concrete steps, in order:

### Step 1: Unify `:default` handling
- Add `:default #{java.lang.Object nil}` to the nemesis type registry (in `nemesis.types/init!`)
- Remove the hardcoded `(= :default t)` branch from `nemesis.types/classes`
- Change `nemesis.impl.registry/class-extensions` to pass `(t/get-reg)` instead of `t/classes` as resolver
- This eliminates the class-resolver indirection introduced during migration
- Test carefully — `:default` dispatch is used by every generic with a default case

### Step 2: Migrate `thing_parse-impl-cases`
- The last un-delegated function in `nemesis.impl.forms`
- It calls `reg/get-spec!` to look up method/protocol names
- The poly version (`poly.compiler.forms/thing_parse-impl-cases`) takes `compiler` as first arg
- Delegate via `(poly.forms/thing_parse-impl-cases (current-compiler) impl)`
- Update `thing` to use the delegated version

### Step 3: Consider `nemesis.state` simplification
- The dynamic var `*expansion-state*` + dual-target atom is the core state mechanism
- Evaluate whether it could hold a poly compiler value instead of raw maps
- This is the "final boss" of the migration — approach carefully
- Options: (a) keep as-is (pragmatic), (b) replace with compiler atom, (c) store specs in var metadata
- Document the decision either way

### Step 4: Begin design work on extension modes
- With the cleaner architecture, the open design thread about extend/tune/patch modes becomes tractable
- Add a `:mode` field to generic specs (set at `defg` time)
- Implement checks in `poly.functions.spec/merge-cases` based on mode
- Start with just `extend` mode (no override allowed) as the simplest addition

### Invariants
- All tests must pass after every change
- Poly modules must remain pure (no global state reads)
- nemesis public API (`nemesis.core` macros) must not change
- Generated code must reference `nemesis.core/prototypes`, not `poly.core/prototypes`
