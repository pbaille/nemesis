---
type: session
tags: [extension-modes, fork-semantics, macro-semantics, poly-refactoring, cljs-compat]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T08-04-09-764Z_60f25006-f1d5-4dac-8c1f-f4af750728af.jsonl
---

# Rename modes and refactor fork semantics with default refine

Completed a systematic refactoring of nemesis extension modes: renamed `:tune` → `:refine` as the new default, changed `:patch` → `:override`, and fixed fork semantics to reset cloned generics to `:override` mode. Updated core implementation across spec/parse layers, fixed all four try tests (CLJ and CLJS), and resolved the fork-mode-inheritance question. Marked forking-semantics as effectively complete pending final documentation cleanup.

## Key Points

- Renamed extension modes: `:tune` → `:refine` (default), `:patch` → `:override` throughout poly.functions.spec/parse
- Fork now resets mode to `:override` on clone to prevent inherited restrictions from breaking extensions
- Updated all four CLJ tries and fixed CLJS compatibility (shadow-cljs compile 0 warnings)
- Fork semantics sub-questions resolved as non-issues: runtime state is isolated per fork, structural sharing is unnecessary, fork-of-fork already works (tried/three.cljc proof)
- Marked forking-semantics thread complete; identified predicate-guards as sole remaining active design thread

## Files

- `src/poly/functions/spec.clj`
- `src/poly/functions/parse.clj`
- `src/poly/tries/one.cljc`
- `src/poly/tries/four.cljc`
- `.knowledge/notes/three-extension-modes.md`
- `.knowledge/notes/forking-semantics.md`
- `.knowledge/notes/runtime-state-facilities.md`
- `.knowledge/notes/state-bookkeeping-needs.md`
- `.knowledge/maps/open-threads-map.md`
- `.knowledge/self/goals.md`
