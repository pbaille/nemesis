---
type: task
tags: [extension-modes, tune, sealed, extend, type-hierarchy, compile-time-check]
status: done
---

# Implemented `:tune` and `:sealed` extension modes + tightened `:extend`

## What was done

### 1. `check-tune-mode` in `poly.functions.spec`
- Blocks direct overrides: if `[type arity]` already exists in effective-cases, throws
- Allows specialization: adding `:vec` when `:coll` covers it is fine (`:vec` is a new `[type arity]` pair)
- Simple check — identical to the old `check-extend-mode`

### 2. Tightened `check-extend-mode`
- Now also checks if the new type is a **child** of an existing type using `types/parents`
- This makes `:extend` strictly safer than `:tune` — it blocks specialization too
- Required the type registry, which is now threaded through

### 3. `:sealed` mode
- Throws `ex-info` immediately on any `generic+` attempt — no case-level checks needed

### 4. Type registry threading
- `compiler/extend-function` passes `(:types compiler)` through as `:type-registry` kwarg
- `fn.reg/extend-spec` accepts and forwards `:type-registry`
- `fn.spec/merge-cases` accepts `:type-registry` and passes to mode checks
- Clean kwargs — only flows where needed, doesn't pollute unrelated code paths

### 5. Tests in `tries/four.cljc`
- `:tune` specialization works (`:vec` under `:coll`) ✓
- `:tune` new type works (`:number` added freely) ✓
- `:tune` blocks `:vec` override (after `:vec` was specialized) ✓
- `:tune` blocks `:coll` override (direct impl exists) ✓
- `:extend` blocks specialization (`:vec` when `:coll` covers it) ✓
- `:sealed` blocks all extensions ✓
- All CLJ tries pass, CLJS shadow-cljs compiles clean

## Files modified
- `src/poly/functions/spec.clj` — added `check-tune-mode`, tightened `check-extend-mode`, updated `merge-cases` with `:sealed`/`:tune` dispatch and `:type-registry` kwarg
- `src/poly/functions/registry.clj` — `extend-spec` accepts `:type-registry`
- `src/poly/compiler/core.clj` — `extend-function` passes `:types` through
- `src/poly/tries/four.cljc` — comprehensive tests for all modes

## Design insight
The distinction between `:extend` and `:tune` turns on how `effective-cases` works. Cases store their exact type keyword (`:coll`, not `:vec`). So specialization (adding `:vec` when only `:coll` exists) is already a "new" pair — no special logic needed in `:tune`. The trick is making `:extend` stricter: it uses `types/parents` to detect that `:vec` is covered by `:coll` and blocks it.

## Deferred
- Fork mode inheritance (should `clone` preserve or reset `:mode`?) — noted in three-extension-modes note, not implemented
