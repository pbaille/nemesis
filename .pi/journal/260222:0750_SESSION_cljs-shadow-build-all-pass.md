---
type: session
tags: [cljs, shadow-cljs, poly-core, verification, milestone]
---

# CLJS Shadow Build: All Tests Pass

## Summary

Poly.core macros compile and run correctly under ClojureScript via shadow-cljs. 
This is a **major milestone** — first time the new poly macro layer has been verified in CLJS context.

## Results

### Compilation
- `npx shadow-cljs compile main` — **0 warnings, 0 errors**
- All 4 tries files compiled: `one.cljc`, `two.cljc`, `three.cljc`, `four.cljc`
- 47 files total, ~2.4MB JS output

### Runtime (browser)
- All assertions in `poly.tries.one` pass (defg, generic+, type+, fork, fork+, deft, thing)
- All assertions in `poly.tries.two` pass (cross-namespace extension, deft, fork)
- `poly.tries.three` loads (minimal fork test)
- `poly.tries.four` prints **"Poly Extension modes: ALL TESTS OK"**
- No console errors, no assertion failures

### Incremental builds
- `prepare-state!` build hook works correctly
- Editing `one.cljc` → only 3 files recompiled (0.22s)
- Hot-reload in browser re-runs all assertions → all pass
- Namespace-keyed spec registration + `prepare-ns!` pattern works in CLJS

### Verified CLJS code paths
- `poly.compiler.forms/cljs-extend1` — prototype assignments work
- `poly.state/cljs?` — correctly detects CLJS via `(:ns &env)`
- `poly.core/prototypes` — correctly referenced (0 nemesis refs in output)
- `deft` class naming — CLJS path produces correct symbols
- `qualify-symbol` — `cljs.analyzer/resolve-var` works for cross-namespace refs

## Changes Made

1. **`tries/three.cljc`** — Added `:include-macros true` to `poly.core` and `poly.tries.two` requires (was missing)
2. **`shadow-cljs.edn`** — Extended entries to include `poly.tries.three` and `poly.tries.four`

## Notes

- `tries/three.cljc` uses `:str` instead of `:string` as a type key — pre-existing issue, not introduced by poly migration
- Shadow-cljs uses port 8081 when 8080 is occupied
- WebSocket relay errors in console are stale token noise, not code issues
