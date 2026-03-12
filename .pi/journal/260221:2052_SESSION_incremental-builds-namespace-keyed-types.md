---
type: session
tags: [incremental-builds, namespace-keyed-registry, backward-compat-removal, type-contributions, poly-migration]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-39-57-828Z_35e5df21-68fa-4a89-9007-d8f00fbe1e18.jsonl
---

# Incremental builds: remove backward compat, namespace-key types

Completed the incremental build infrastructure by removing backward compatibility paths from spec/registry functions, implementing namespace-keyed type contribution tracking, and wiring type cleanup into the prepare-ns! lifecycle. All functions now require explicit extension-ns parameters, type contributions are stored separately per namespace with computed effective views, and both function specs and types flow through the incremental build model for hermetic isolation.

## Key Points

- Removed backward compatibility: merge-cases, extend-spec, extend-function now require extension-ns as positional parameter instead of optional keyword
- Implemented namespace-keyed type registry in poly.types.core with pure functions (make-type-state, register-type-contribution, effective-types, remove-type-contributions)
- Fixed defg to pass initialized spec to forms/declaration, preventing case loss from effective-cases computation
- Added type contribution cleanup to prepare-ns! and remove-ns-contributions, enabling hermetic namespace isolation for both functions and types
- Added comprehensive tests: poly.types.core_test (5 tests, 33 assertions) and incremental build type scenarios (4 sub-scenarios covering rebuild, isolation, and changes)

## Files

- `src/poly/functions/spec.clj`
- `src/poly/functions/registry.clj`
- `src/poly/compiler/core.clj`
- `src/poly/types/core.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/impl/forms.clj`
- `src/nemesis/core.clj`
- `src/nemesis/state.clj`
- `src/nemesis/types.clj`
- `test/poly/types/core_test.clj`
- `test/poly/incremental_build_test.clj`
- `test/poly/functions/spec_test.clj`
- `shadow-cljs.edn`
