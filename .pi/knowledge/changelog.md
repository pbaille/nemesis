# Knowledge Changelog

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
