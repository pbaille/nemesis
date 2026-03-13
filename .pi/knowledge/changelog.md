# Knowledge Changelog

## 2026-03-13T06:48 — P0 bug fixes applied (knowledge-daemon-37)

- worker-p0-fixes fixed BUG-1 (Boolean class), BUG-2 (isa fallback), BUG-3 (childof/parentof)
- Updated `modules/types.md` to mark all three as FIXED
- BUG-4 (extension ordering) remains unfixed

## 2026-03-13T06:36 — Bug hunter findings (knowledge-daemon-37)

- Updated `modules/types.md` with 3 confirmed bugs (Boolean crash, isa silent nil, childof false positives) + dead code
- Updated `modules/functions.md` with BUG-4 (alphabetical extension ordering)
- Source: bug-hunter report (5 bugs: 1 crash, 2 high, 2 medium severity)

## 2026-03-13T06:30 — Docs/structure critic findings (knowledge-daemon-37)

- Updated `modules/utils.md` with dead code + structure note (grab-bag concern)
- Updated `learnings.md`: extension ordering surprise, guard runtime overhead, undocumented types API
- Source: docs-structure-critic report

## 2026-03-13T06:29 — Dead code audit findings (knowledge-daemon-37)

- Updated `modules/state.md` with dead code section (5 dead vars)
- Updated `learnings.md`: confirmed test files orphaned, added dead code learning
- Source: dead-code-detective report (24 dead items across 8 files)

## 2026-03-13T06:28 — Test audit findings (knowledge-daemon-37)

- Updated `testing.md` with coverage gaps from test-auditor report
- Key gaps: thetis.types entirely untested, implements? zero coverage, CLJS coverage illusory
- Structural issue: tries use raw assert instead of clojure.test framework

## 2026-03-12T13:01 — Test alias added (knowledge-daemon-37)

- `deps.edn`: New `:test` alias added (runs all tries namespaces via `clj -M:test`)
- `README.md`: Development commands updated to use `:test` alias
- Updated `testing.md`: commands section and removed "no test alias" known issue
- Agents: worker-test-alias, readme-fixer

## 2026-03-12 — Initial mapping (project-mapper-5)

**Scope**: Full project mapping — all source files read, test files read, config files read.

**Files produced**:
- `project.md` — identity, vitals, entry points, architecture, commands
- `structure.md` — annotated directory map with line counts
- `conventions.md` — code style, naming, state management, extension modes
- `testing.md` — frameworks, commands, coverage table, known issues
- `dependencies.md` — minimal deps table
- `modules/core.md` — public macro API
- `modules/state.md` — global compile-time state
- `modules/compiler.md` — pure compiler + code generation
- `modules/functions.md` — parse, registry, spec manipulation
- `modules/types.md` — type keyword system + hierarchy
- `modules/utils.md` — shared utilities + defmac
- `modules/tries.md` — integration tests
- `learnings.md` — non-obvious findings

**Key findings**:
- Test files still use old `poly.*` namespace (rename incomplete)
- Extremely minimal dependencies (only Clojure + ClojureScript)
- Sophisticated incremental build support via namespace-keyed contributions
- Pure/impure architectural split well maintained

**Confidence**: high for all modules (every source file was read in full)
