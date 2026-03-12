# Testing

## Frameworks
- **clojure.test**: Unit tests in `test/poly/` (3 files, ~613 lines)
- **Assertion-based**: Integration tests in `src/thetis/tries/` (5 files, ~652 lines)
- **shadow-cljs**: CLJS tests via browser compilation

## Commands
```bash
# Integration tests (CLJ) — load tries namespaces, assertions run on load
clj -M -e "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"

# Unit tests (CLJ) — clojure.test runner
clj -M -m clojure.test poly.types.core-test poly.functions.spec-test poly.incremental-build-test

# CLJS tests — compiles + runs tries via shadow-cljs
npx shadow-cljs compile main
```

## Test Coverage
| Area | Test file | What's tested |
|------|-----------|---------------|
| Type registry | `test/poly/types/core_test.clj` | Contribution registration, removal, idempotent rebuild |
| Function specs | `test/poly/functions/spec_test.clj` | Init, merge, ordering, clone isolation, ns removal |
| Incremental builds | `test/poly/incremental_build_test.clj` | Full rebuild scenarios, fork survival, type+function combined |
| Core features | `tries/one.cljc` | defg, generic+, type+, fork, fork+, deft, thing, multi-arity |
| Cross-ns | `tries/two.cljc` | deft across namespaces, thing |
| Fork | `tries/three.cljc` | Fork + override |
| Extension modes | `tries/four.cljc` | sealed, extend, refine, override — compile-time enforcement |
| Guards | `tries/five.cljc` | defguard, predicate dispatch, guard + fork interaction |

## Known Issues
- **Test namespace mismatch**: Unit tests in `test/poly/` still use old `poly.*` namespace (project was renamed from poly → thetis). Imports reference `poly.types.core`, `poly.functions.spec`, etc. These will break unless there's a source path alias or the old namespaces still exist somewhere.
- **No test runner alias**: No `:test` alias in deps.edn
