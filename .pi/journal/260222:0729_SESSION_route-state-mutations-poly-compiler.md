---
type: session
tags: [state-mutations, poly-compiler, functional-boundaries, refactoring, architecture]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-52-19-501Z_27173321-567b-47e8-9271-86002edc5885.jsonl
---

# Route nemesis state mutations through poly compiler

Completed systematic refactoring to eliminate raw state mutations from nemesis library code. All state updates now route through pure poly.compiler.core functions instead of direct atom swaps. Implemented `register-type-contribution` in poly compiler, rewired registry and core mutation points, fixed namespace alias resolution issues, and verified all tests pass.

## Key Points

- Added `register-type-contribution` function to poly.compiler.core for atomic type-state updates
- Routed 4 stateful operations through compiler: `register-spec!`, `extend-spec!`, `clone-spec!`, `remove-ns-contributions!`
- Fixed namespace alias resolution by qualifying names at nemesis boundary before passing to poly
- Reduced atomic swap calls: `remove-ns-contributions!` went from 3 swaps to 1, `register-type` from 2 to 1
- All 19 unit tests and 4 integration test namespaces pass with no regressions
- Clarified that poly is intended to replace nemesis entirely (not just be internal scaffolding)

## Files

- `src/poly/compiler/core.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/core.clj`
