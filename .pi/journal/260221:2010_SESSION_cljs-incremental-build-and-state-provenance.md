---
type: session
tags: [cljs-compilation, namespace-provenance, incremental-builds, state-management, open-thread]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T18-44-53-393Z_b849e114-3349-4c75-bc44-8cd34085c069.jsonl
---

# CLJS compilation model and namespace provenance for incremental builds

Audited the three "dead" state keys (:guards, :namespaces, :prototypes) and discovered they were intentional placeholders for real design problems rather than unused code. Traced through CLJS compilation flow with shadow-cljs, identifying that `:cache-level :off` defeats incremental compilation entirely and that namespace provenance tracking is critical to supporting incremental CLJS builds—a core project goal. The session revealed fundamental gaps in how state is rebuilt during recompilation vs how it could be preserved across incremental builds.

## Key Points

- Three state keys (:guards, :namespaces, :prototypes) were design placeholders, not dead code—they track real concerns: namespace provenance, predicate-based type dispatch, and compile-time mirror of runtime implementations
- CLJS compilation flow: shadow-cljs calls reset-state! on every build (nuking all state), then recompiles all namespaces, incrementally rebuilding compile-time state through macro expansion. `:cache-level :off` disables incremental caching entirely.
- Namespace provenance ledger (:namespaces key) would enable incremental CLJS builds by allowing state reconstruction from unchanged namespaces instead of full reset-and-rebuild
- Expanded type-level sync (types-syncronisation function) exists but is unused—register-type uses class-level sync instead (extend-class), representing incomplete refactoring
- :fns → :functions alignment and current-compiler simplification are correct mechanical changes that don't affect core functionality
- Extension mode checking works today but has no namespace tracking, limiting error diagnostics and hot-reload debugging

## Files

- `src/nemesis/state.clj`
- `src/nemesis/types.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/impl/forms.clj`
- `src/nemesis/core.clj`
- `src/nemesis/core.cljs`
- `src/poly/state.clj`
- `src/poly/compiler/forms.clj`
- `.knowledge/notes/state-bookkeeping-needs.md`
- `.knowledge/notes/state-shape-aligned-with-poly-compiler.md`
