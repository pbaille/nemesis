---
type: session
tags: [poly-migration, macros, types, poly-core, integration-tests]
status: complete
---

# poly.core macros and types initialization — built and passing

## What happened

Built the complete `poly.core` macro layer and supporting infrastructure. All poly integration tests pass, all nemesis tests still pass, both systems run concurrently without interference.

## Files created

### `src/poly/core.clj` — Full macro layer
All macros ported from `nemesis.core`:
- `defg`, `generic+`, `fork`, `fork+`, `type+`, `thing`, `implements?`, `register-type`, `deft`
- Build hooks: `reset-state!`, `prepare-state!`
- Internal helpers: `current-compiler`, `prepare-ns!`, `get-spec`, `get-spec!`
- Uses `poly.utils.misc/defmac` for the function+macro dual pattern

Key differences from nemesis.core:
- No `nemesis.impl.registry` middleman — macros call `poly.state/swap!` with `poly.compiler.core` functions directly
- No `with-nemesis-prototypes` needed — `*prototypes-sym*` defaults to `'poly.core/prototypes`
- Uses `poly.utils.misc/defmac` instead of `nemesis.impl.utils/defmac`

### `src/poly/core.cljs` — ClojureScript stub
Just `(defonce prototypes (atom {}))`.

### `src/poly/types.clj` — User-facing type operations
Hierarchy queries, predicate compilation, registry access. Reads from `poly.state`.

### `src/poly/tries/{one,two,three,four}.cljc` — Integration tests
Direct ports of `nemesis/tries/*` with `nemesis.core` → `poly.core`, `nemesis.state` → `poly.state`.

## Files modified

### `src/poly/utils/misc.clj`
- Added `doall-rec` — realizes nested lazy seqs (critical for dynamic var safety in macros)
- Added `defmac` — the dual function+macro definition pattern, ported from `nemesis.impl.utils`
- Added `clojure.walk` require

### `src/poly/compiler/forms.clj`
- Added `*generic+-sym*` dynamic var (defaults to `'poly.core/generic+`)
- Changed `implement` to use `*generic+-sym*` instead of hardcoded `'nemesis.core/generic+`

### `src/nemesis/impl/forms.clj`
- `with-nemesis-prototypes` now also binds `*generic+-sym*` to `'nemesis.core/generic+`
- `implement` wrapped with `with-nemesis-prototypes`

### `src/poly/state.clj`
- Added requires for `poly.types.core` and `poly.types.data`
- Added `init-types!` function — populates CLJ and CLJS type registries at load time
- Called `(init-types!)` at load time

### `src/poly/functions/registry.clj`
- `get-class-cases` now takes 3 args: `[reg type-registry class]` — was missing type registry

### `src/poly/compiler/core.clj`
- `get-class-cases` now passes `(:types compiler)` to `fn.reg/get-class-cases`

## Bug found and fixed

`poly.functions.registry/get-class-cases` was calling `spec/class-extensions` with only the spec (1 arg), but the function requires 2 args (spec + type-registry). This worked in nemesis because `nemesis.impl.registry/get-class-cases` had its own implementation that threaded `(t/get-reg)` through. The pure module needed the type registry passed explicitly.

## Test results

```
Unit tests:       19 tests, 92 assertions, 0 failures
Nemesis tries:    one ✓, two ✓, three ✓, four ✓
Poly tries:       one ✓, two ✓, three ✓, four ✓
Concurrent load:  no interference between nemesis and poly state
```

## Migration status

Steps from `260222:0724_DESIGN_poly-succession-migration-path.md`:
- ✅ Step 1: Complete `poly.state` (done last session + types init this session)
- ✅ Step 2: Move macros to `poly.core` (done this session)
- ✅ Step 3: Absorb `nemesis.impl.forms` — not absorbed, but unnecessary. `poly.core` calls `poly.compiler.forms` directly, no bridge needed.
- ✅ Step 4: Absorb `nemesis.types` — `poly.types` created as independent namespace
- ⬜ Step 5: Delete `nemesis.*` (or keep as deprecation shim) — not yet

## What's next

- Step 5: Delete nemesis or create thin re-export shim
- ClojureScript testing (shadow-cljs build with poly.core)
- Consider whether `nemesis.impl.utils` has anything else poly needs
