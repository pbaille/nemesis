---
type: task
tags: [predicate-guards, type-system, implementation, complete]
status: done
---

# Predicate guards: designed and implemented

## What was done

Designed and implemented predicate guards for poly — runtime dispatch for non-class types like `:positive` (number + pos?) and `:non-empty-vec` (vector + seq).

### Design decisions

1. **Two-phase dispatch**: Generics with guards check a runtime `guard-impls` atom first, then fall through to protocol dispatch. Only generics with guard cases pay any runtime cost.

2. **Open extension via `generic+`**: Guard implementations are stored in a runtime atom (like `prototypes`), so `generic+` can add guard cases without re-emitting the entire function. The function definition is only re-emitted when a generic gets its *first* guard case.

3. **Guards NOT in type registry**: Originally planned to register guards as pseudo-types. Abandoned — creates circular hierarchy issues. Instead, extension mode checks use a separate `guard-parent-types` function.

4. **Extension mode integration**: `:positive` is treated as a child of `:number` for mode checks. `:extend` blocks specialization (adding `:positive` when `:number` covers it). `:refine` allows it.

### Files changed

- `src/poly/core.clj` — `defguard` macro, `guard-impls` atom
- `src/poly/core.cljs` — `guard-impls` atom
- `src/poly/state.clj` — guard-state
- `src/poly/types/core.clj` — guard state management
- `src/poly/compiler/core.clj` — register-guard, remove-guard-contributions
- `src/poly/compiler/forms.clj` — guard registration code gen, two-phase function-definition
- `src/poly/functions/parse.clj` — guard case tagging
- `src/poly/functions/spec.clj` — guard-parent-types, exclude guards from class-extensions
- `src/poly/functions/registry.clj` — guards kwarg
- `src/poly/tries/five.cljc` — comprehensive test suite
- `shadow-cljs.edn` — five.cljc added

### Tests

- CLJ: all 5 tries pass ✅
- CLJS: shadow-cljs compile succeeds (1 expected redef warning) ✅
- Tests cover: basic guards, multiple guards, generic+ with guards, extension modes + guards, multi-arity, fork, fork+, aggregate base types (`:coll`)

### Vault updates

- Created `.knowledge/notes/predicate-guards-design.md`
- Updated `state-bookkeeping-needs.md`, `open-threads-map.md`, `goals.md`
