---
type: task
tags: [cljs, incremental-builds, spec-registration, poly-migration, compiler-as-value, shadow-cljs]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T18-44-53-393Z_b849e114-3349-4c75-bc44-8cd34085c069.jsonl
---

# Design spec registration model for CLJS incremental builds

## Context

### The Problem

Nemesis compile-time state (the `state` atom in `nemesis.state`) accumulates as macros expand: `defg` registers specs, `generic+` extends them, `fork` clones them, `register-type`/`deft` mutates the type registry. On CLJS (shadow-cljs), this creates a fundamental conflict:

- `reset-state!` (a `:compile-prepare` build hook) nukes all compile-time state before each build
- Shadow-cljs then recompiles namespaces — but in incremental mode, only *changed* files re-expand macros
- Unchanged namespaces' `defg`/`generic+` registrations are **lost** from compile-time state
- Result: compilation fails because extensions reference specs that no longer exist

**Current workaround**: `shadow-cljs.edn` uses `:cache-level :off` to force full recompilation every time. This defeats incremental builds entirely.

### Why a Simple Replay Won't Work

The core issue is **non-idempotent mutations**. `extend-spec!` works by concatenating new cases onto existing specs:

```clojure
;; poly.functions.spec/merge-cases
(update original-spec :cases (partial concat (:cases extension-spec)))
```

Re-running an extension doubles the cases. You can't naively replay namespace contributions.

Additionally, **specs are cross-namespace entangled**:
- `fork` clones a spec from another namespace (stores `:cloned-from`)
- `generic+` mutates a spec defined in another namespace (prepends cases)
- `type+` / `deft` → `register-type` → `extend-class` re-emits protocol extensions for all generics touching a class

### What Works Today

The **runtime prototype atom** architecture is already CLJS-friendly: all protocol implementations read through `(get-in @prototypes [type 'generic-name arity])`, so emitted JS is self-consistent regardless of compilation order. The problem is **purely compile-time**.

### Current State Shape

After the `:fns` → `:functions` alignment (commit `ad8dc1e`), nemesis state matches the poly compiler shape:

```clojure
;; nemesis.state/state atom
{:clj  {:functions {} :types {}}
 :cljs {:functions {} :types {}}}

;; poly compiler value  
{:functions {} :types {}}
```

`current-compiler` in `nemesis.impl.forms` just reads the state slice and adds expansion context:
```clojure
(defn current-compiler []
  (assoc (state/get) :expansion {:env (state/env) :form (state/form)}))
```

### Mutation Surface

Only 3 places write to state:
1. `nemesis.impl.registry/register-spec!` → `(state/swap! assoc-in [:functions fullname] spec)` — **idempotent** (simple assoc)
2. `nemesis.impl.registry/extend-spec!` → calls `merge-cases` then `register-spec!` — **NOT idempotent** (concat cases)
3. `nemesis.core/register-type` → `(state/swap! update :types ...)` — mutates type registry

### Key Architectural Insight

`register-spec!` is just an `assoc-in` — it's idempotent. The non-idempotency comes from `extend-spec!` which reads the current spec, concatenates cases, then writes back. If extensions produced **complete new specs** rather than deltas, every mutation would be a simple `assoc`.

## Files

Core state and mutation:
- `src/nemesis/state.clj` — state atom, `*expansion-state*`, platform switching
- `src/nemesis/impl/registry.clj` — `register-spec!`, `extend-spec!`, `clone-spec!`
- `src/nemesis/core.clj` — public macros, `reset-state!`, `register-type`

Poly pure layer:
- `src/poly/functions/spec.clj` — `merge-cases`, `clone`, `class-extensions`
- `src/poly/functions/registry.clj` — `add-spec`, `extend-spec`, `clone-spec`
- `src/poly/compiler/core.clj` — pure compiler update functions
- `src/poly/compiler/data.clj` — `make-compiler`, initial compiler values

Code generation:
- `src/nemesis/impl/forms.clj` — `current-compiler`, code emission (CLJ vs CLJS paths)
- `src/poly/compiler/forms.clj` — `extension-form` (CLJ: `extend`, CLJS: prototype mutation via `js*`)

CLJS config:
- `shadow-cljs.edn` — build config with `:cache-level :off`, `:build-hooks`, `:cache-blockers`

Tests:
- `src/nemesis/tries/{one,two,three,four}.cljc` — exercise `defg`, `generic+`, `fork`, `fork+`, `deft`, `type+`, extension modes

Vault notes:
- `.knowledge/notes/cljs-incremental-build-problem.md` — full analysis with strategy options
- `.knowledge/notes/state-bookkeeping-needs.md` — the three original placeholder keys and their intent
- `.knowledge/notes/state-shape-aligned-with-poly-compiler.md` — the state/compiler unification

## Task

Design a spec registration model where CLJS incremental builds are possible — i.e., shadow-cljs can use caching and only recompile changed namespaces without corrupting compile-time state.

### 1. Analyze the mutation model

Map every compile-time state mutation to understand exactly what needs to be preserved across incremental builds:

| Macro | State mutation | Idempotent? | Cross-ns? |
|-------|---------------|-------------|-----------|
| `defg` | `register-spec!` (assoc) | ✅ | No |
| `generic+` | `extend-spec!` (read-concat-write) | ❌ | Yes — extends specs from other ns |
| `fork` | `clone-spec!` (read original, assoc clone) | ✅ | Yes — reads spec from other ns |
| `register-type` | `swap! update :types` | ❌ (additive) | No |
| `deft` | `register-type` + `defg` for cast fn | combo | No |
| `type+` | No state mutation (emits `generic+` calls) | N/A | Yes |

### 2. Design idempotent spec registration

The key idea: **separate the "declaration cases" from "extension cases" in the spec data model.** Instead of `merge-cases` concatenating onto a flat `:cases` vector, store cases in batches tagged with their source namespace:

```clojure
;; Instead of:
{:cases [case1 case2 case3 case4]}  ;; flat, order-dependent, non-idempotent

;; Consider:
{:declaration-cases [case1 case2]                          ;; from defg
 :extension-cases {"ns.b" [case3] "ns.c" [case4]}}        ;; from generic+, keyed by ns
```

This makes re-registering an extension from ns.b idempotent — it replaces the existing entry rather than appending. The effective case list is computed by concatenating declaration + extensions in a deterministic order.

Explore this direction. Key questions:
- How does case **precedence** work with batched extensions? (Currently: first match wins, order = concat order)
- Does the extension ordering need to be explicit? (Compile order = require order, which is deterministic)
- How does `:cloned` interact with batched cases?
- Can the type registry mutations (`register-type`) also be made idempotent?

### 3. Design the incremental build lifecycle

With idempotent registration, the shadow-cljs lifecycle could become:

```
1. Build starts
2. reset-state! saves current state (instead of nuking)  
3. Shadow identifies changed namespaces
4. For each changed ns: remove its contributions from state
5. Recompile changed namespaces (macros re-register into state)
6. State is now consistent
```

Or alternatively:
```
1. Build starts  
2. Don't reset — state persists from last build
3. Shadow recompiles changed namespaces
4. Macros overwrite (idempotent) their registrations
5. State is consistent because registration is idempotent
```

Design which approach works. Consider:
- What about **deleted** definitions? (defg removed from source file)
- What about **renamed** definitions?
- Can the state be serialized/deserialized for cold starts?

### 4. Prototype to validate

Implement the new registration model in the poly layer (pure functions, no global state). Write tests that simulate the incremental build scenario:

```clojure
;; Simulate: compile ns-a, then ns-b (extends ns-a's generic), 
;; then recompile only ns-b with changes
;; Verify: state is correct, no duplicate cases
```

### 5. Integrate with nemesis

If the model works, wire it into `nemesis.impl.registry` and update `shadow-cljs.edn` to allow caching.

### Invariants
- All existing tests must pass (`nemesis.tries.one` through `four`)
- Poly modules remain pure (no global state)
- Extension modes (`:extend`, `:tune`, `:patch`) continue to work correctly
- Fork isolation is preserved (extending a fork doesn't affect the original)
- Case precedence semantics unchanged (first match wins)
- Run tests: `clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (require (quote nemesis.tries.four)) (println "ALL TESTS OK"))'`

### Open Questions
- Should the poly compiler itself know about namespaces, or is this a nemesis-layer concern?
- Is shadow-cljs's `:cache-blockers` mechanism sufficient if registration is idempotent, or do we need the build hook too?
- How does `types-syncronisation` (currently unused) fit into this picture?
