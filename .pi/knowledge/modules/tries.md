# Module: tries

- **Path**: `src/thetis/tries/` (5 `.cljc` files, ~652 lines)
- **Confidence**: medium
- **Covers**: `src/thetis/tries/one.cljc` (read fully), `src/thetis/tries/two.cljc` through `five.cljc` (skimmed)

## Purpose
Integration test suite and usage examples. Each file exercises a specific feature area. All tests are assertion-based (not clojure.test) — they run on namespace load and throw on failure. Cross-platform via `.cljc`.

## File Breakdown
| File | Lines | Tests |
|------|-------|-------|
| `one.cljc` | 229 | Core: defg, generic+, multi-arity, type+, fork, fork+, deft, thing |
| `two.cljc` | 61 | Cross-namespace: deft, thing with imports from `one` |
| `three.cljc` | 14 | Fork + override across namespaces |
| `four.cljc` | 224 | Extension modes: sealed, extend, refine, override with compile-time error testing |
| `five.cljc` | 124 | Predicate guards: defguard, guard dispatch, guard + fork, guard + generic+ |

## Notable Patterns
- Tests in `one.cljc` define reusable generics (`g1`, `g2`, `g3`, `sip`, `valid`) and types (`bub`, `bib`, `point`) used by later tries
- CLJS compile-time error testing uses `#?(:clj ...)` reader conditionals to run `macroexpand` checks only on CLJ
- `four.cljc` extensively tests that mode violations throw at compile time

## Connections
- Imports: `thetis.core`, each other (sequential dependency: one → two → three)
- Referenced by: shadow-cljs.edn (`:entries`), README test commands
