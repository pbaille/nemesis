---
type: session
tags: [incremental-builds, compile-time-state, idempotency, spec-registration, shadow-cljs]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-10-49-886Z_e845fd8f-6817-4947-ad33-59751d254b7b.jsonl
---

# Namespace-keyed spec registration for incremental builds

Implemented idempotent spec registration by storing extension cases in namespace-keyed maps instead of flat vectors. This enables incremental ClojureScript builds where changed namespaces recompile without corrupting compile-time state. Added per-namespace cleanup via `prepare-ns!` macro hook and completed poly layer refactoring with namespace context threading throughout.

## Key Points

- Changed spec data model from flat `:cases` vector to `:declaration-cases` + `:extension-cases` map keyed by namespace, computing effective cases on-demand
- Implemented namespace-scoped cleanup: `prepare-ns!` removes a namespace's contributions from specs and types before re-expansion, enabling idempotent re-registration
- Added `remove-ns-contributions!` to both poly.functions and nemesis registries to support cleanup across compile-time state
- Threaded `:extension-ns` parameter through all registration functions (registry, compiler, core macros) to track provenance
- Replaced build hook from `reset-state!` (full reset) to `prepare-state!` (selective cleanup), enabling incremental builds in shadow-cljs
- Created 13 new tests (57 total assertions) validating spec isolation, idempotency, fork behavior, and incremental build semantics

## Files

- `src/poly/functions/spec.clj`
- `src/poly/functions/registry.clj`
- `src/poly/compiler/core.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/state.clj`
- `src/nemesis/core.clj`
- `test/poly/functions/spec_test.clj`
- `test/poly/incremental_build_test.clj`
- `.knowledge/notes/namespace-keyed-spec-registration.md`
- `.knowledge/notes/cljs-incremental-build-problem.md`
