---
type: session
tags: [poly-refactoring, macro-layer, nemesis-deletion, bug-fix, integration-tests]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T06-29-36-653Z_0288fef7-9e45-4467-89b9-12869d73dc56.jsonl
---

# Poly core macros and nemesis deletion completed

Completed the poly refactoring by implementing the full macro layer (defg, generic+, fork, type+, deft) with supporting utilities, porting integration tests from nemesis, fixing a critical type-registry threading bug in the pure module layer, and then removing all nemesis source code for a clean break. All poly and unit tests pass with zero regressions. Next step is ClojureScript verification via shadow-cljs.

## Key Points

- Built poly.core with all 8 user-facing macros and supporting infrastructure (defmac, doall-rec, dynamic var binding for generic+ symbol)
- Ported nemesis integration tests (four.cljc) to poly.tries with full passing compatibility
- Fixed critical bug: poly.functions.registry/get-class-cases was missing type-registry argument to spec/class-extensions (latent because nemesis used global state)
- Removed all 13 nemesis source files including core, impl, types, state, tries — confirmed zero remaining nemesis references in poly or tests
- Verified end-to-end: 19 unit tests + 4 poly integration tests all pass, no interference between removed nemesis and active poly systems

## Files

- `src/poly/core.clj`
- `src/poly/core.cljs`
- `src/poly/types.clj`
- `src/poly/tries/one.cljc`
- `src/poly/tries/two.cljc`
- `src/poly/tries/three.cljc`
- `src/poly/tries/four.cljc`
- `src/poly/utils/misc.clj`
- `src/poly/compiler/forms.clj`
- `src/poly/compiler/core.clj`
- `src/poly/functions/registry.clj`
- `src/poly/functions/spec.clj`
- `test/poly/incremental_build_test.clj`
- `shadow-cljs.edn`
- `.knowledge/maps/architecture-map.md`
- `.knowledge/notes/poly-replaces-nemesis.md`
- `.knowledge/notes/poly-migration-strangler-fig.md`
