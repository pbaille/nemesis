---
type: task
tags: [extension-modes, rename, refine, override, default-mode, fork-mode-reset]
status: done
---

# Rename extension modes + default to :refine — COMPLETED

## Changes Made

### Core implementation (`poly.functions.spec`)
- Renamed `check-tune-mode` → `check-refine-mode`, updated docstring + error messages
- Updated `check-extend-mode` error messages: `:tune`→`:refine`, `:patch`→`:override`
- Updated `merge-cases` case dispatch: `:tune`→`:refine`, added explicit `:override nil`, nil fallback → `check-refine-mode`
- `clone` now adds `:mode :override` to merge map (fork mode reset)

### Default mode (`poly.functions.parse`)
- Changed `(or (:mode name-meta) :patch)` → `(or (:mode name-meta) :refine)`

### Test fixes
- `tries/one.cljc`: Added `^{:mode :override}` to `g2` (overrides `:vec` at arity 2 via `generic+`)
- `tries/four.cljc`: Full rewrite — renamed all tune→refine, patch→override, `patchable-fn` now explicit `:override`, added tests for default mode (:refine behavior) and fork mode reset (:sealed parent → :override child)
- `tries/two.cljc`: No changes needed (fork resets to :override, `:symbol` is new type under :refine)
- `tries/three.cljc`: No changes needed (fork chain resets to :override)

### Vault notes
- `three-extension-modes.md` — Full rewrite with new names, spectrum diagram, history section
- `open-threads-map.md` — Updated mode references, marked fork mode inheritance as resolved
- `forking-semantics.md` — Marked "Fork + extension modes" section as RESOLVED
- `self/goals.md` — Updated mode names
- `runtime-state-facilities.md` — Updated `:patch` → `:override` reference

## Test Results
- ✅ `poly.tries.one` — passes
- ✅ `poly.tries.two` — passes
- ✅ `poly.tries.three` — passes
- ✅ `poly.tries.four` — passes (including new default-mode and fork-reset tests)
- ✅ `shadow-cljs compile main` — 0 warnings, 0 errors

## Final Mode Spectrum
```
:sealed ── :extend ── :refine ── :override
  │            │          │          │
no ext    new types   + specialize  anything
          only        subtypes      goes
                      (DEFAULT)
```
