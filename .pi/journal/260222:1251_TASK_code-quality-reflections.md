---
type: task
tags: [code-quality, refactoring, naming, documentation, cleanup]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T11-40-13-798Z_2c9aa137-286d-4bc0-9897-c52b65d81303.jsonl
---

# Code Quality Reflections

## Context

The `poly` → `thetis` namespace rename is complete (all 20 source files, shadow-cljs.edn, README.md). All CLJ and CLJS tests pass, JAR builds correctly. The codebase is now stable enough to do a quality audit.

The codebase is ~1,218 lines across 20 files. Largest files: `compiler/forms.clj` (277), `functions/spec.clj` (179), `utils/misc.clj` (164), `types/core.clj` (164). The public API surface is in `thetis.core` (~215 lines, 9 public defs + macros).

### Architecture recap

```
thetis.core           — Public API: defg, generic+, fork, type+, thing, deft, defguard, etc.
thetis.state          — Mutable compile-time state (atom), expansion context, type init
thetis.compiler/
  core.clj            — Pure compiler queries (get-function, class-cases, clone, extend)
  data.clj            — Compiler data structures (tiny, 19 lines)
  forms.clj           — Code generation: emits protocol defs, extend calls, CLJS compat
thetis.functions/
  parse.clj           — Parses defg/generic+ bodies into structured specs
  registry.clj        — Function spec registry operations
  spec.clj            — Spec manipulation: cases, extension modes, effective-cases
thetis.types/
  core.clj            — Pure type registry operations (hierarchy, resolution)
  data.clj            — Platform type data (CLJ classes, CLJS classes, groups)
thetis.types          — Impure type shell (reads from state)
thetis.utils/
  expansion.clj       — Expansion context helpers (cljs?, qualify-symbol)
  misc.clj            — General utilities, macro helpers, fn-cases normalization
  names.clj           — Naming conventions (arify, protocol/method prefix)
```

### Naming artifacts from the rename

The mechanical `poly.` → `thetis.` sed caught everything, but left some **narrative text** that still says "poly":
- `src/thetis/core.clj:25` — `(tap> "reset poly state")`
- `src/thetis/core.clj:37` — `(tap> "prepare poly state (incremental)")`
- `src/thetis/state.clj:29` — comment: `";; Only thetis.state and thetis.core touch this var."` (correct) but line 30 says `";; Pure poly modules never read this"` — "poly" should be "thetis"

### Coding style observations

1. **`(do :label ...)` sections** — Used extensively as named code blocks in `compiler/forms.clj`, `utils/misc.clj`, `tries/one.cljc`. This is Pierre's organizational convention (like lightweight sections). Not a problem, just notable.

2. **`(comment ...)` blocks** — Two remain:
   - `core.clj:213` — `(comment (state/get :types))` — REPL scratch
   - `functions/parse.clj:116` — `(comment :scratch ...)` — development scratch

3. **Stale "poly" in comments/strings** — The sed replaced namespace-qualified `poly.` but narrative uses of the word "poly" in comments and tap> calls were not caught. These should be updated to "thetis" for consistency.

4. **`utils/misc.clj` is a grab-bag** — 164 lines mixing CLJS JS interop helpers, symbol manipulation, fn-case normalization, macro helpers (`defmac`), and binding pattern utilities. Could potentially be split but it's not large enough to be a real problem.

5. **`compiler/forms.clj` dynamic vars** — `*prototypes-sym*`, `*guard-impls-sym*`, `*generic+-sym*` are `^:dynamic` defs that hold quoted symbols like `'thetis.core/prototypes`. These are used for code generation indirection (testability, thing/fork scenarios). Clean pattern, but worth documenting why they exist.

6. **Docstrings** — Public macros in `core.clj` have docstrings. Internal functions in `compiler/forms.clj`, `functions/spec.clj` have partial docstrings. `types/core.clj` and `utils/misc.clj` have almost none.

7. **Error messages** — `functions/spec.clj:98` references `"poly.core/defg"` which should now be `"thetis.core/defg"`. Wait — the sed should have caught this. Let me note: verify this was updated.

## Files

All source files under `src/thetis/` (20 files):
- `src/thetis/core.clj` — main API, tap> strings with "poly"
- `src/thetis/state.clj` — comment with "poly"
- `src/thetis/compiler/forms.clj` — largest file, dynamic vars, CLJS extend logic
- `src/thetis/functions/spec.clj` — error message referencing defg
- `src/thetis/functions/parse.clj` — has comment block
- `src/thetis/utils/misc.clj` — grab-bag utilities
- `src/thetis/types/core.clj` — pure, no docstrings
- `README.md` — already updated

## Task

Audit the thetis codebase for code quality and produce concrete improvements. Specifically:

### 1. Fix stale "poly" references in narrative text
Grep for any remaining `poly` (not just `poly.`) in source files. Update:
- tap> strings in `core.clj` ("reset poly state" → "reset thetis state")
- Comments in `state.clj` ("Pure poly modules" → "Pure thetis modules")  
- Any error message strings that reference `poly.core/defg` or similar
- Any other narrative mentions

```bash
grep -rn 'poly' src/thetis/ --include='*.clj' --include='*.cljs' --include='*.cljc'
```

### 2. Review and clean up comment blocks
- Evaluate the two `(comment ...)` blocks — keep if useful for REPL development, remove if stale
- Check for any dead code or commented-out experiments

### 3. Docstring audit
Walk through each namespace and assess docstring coverage:
- Public API (`core.clj`) — should be complete (verify)
- `compiler/forms.clj` — document the dynamic vars and the main entry points (`declaration`, `extension`, `thing`)
- `functions/spec.clj` — document `effective-cases`, `init-spec`, extension mode validation
- `types/core.clj` — document public functions (`classes`, `parents`, `childs`, `isa?`, `all-paths`)
- `utils/misc.clj` — at minimum document `defmac` and `fn-cases_normalize`

### 4. Identify any code smells
- Are there functions doing too much?
- Any obvious duplication?
- Are the `(do :label ...)` blocks still well-organized?
- Is the `utils/misc.clj` grab-bag a problem at this scale?

### 5. Produce a summary
Write findings as a brief report. Separate into:
- **Fixes applied** (stale references, dead code removed)
- **Docstrings added** (list which fns got docstrings)
- **Observations** (things that are fine now but worth watching)
- **Suggestions** (optional improvements, not blocking)

Do NOT restructure files or change any behavior. This is a documentation and polish pass only.
