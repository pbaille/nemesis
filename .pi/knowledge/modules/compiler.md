# Module: compiler

- **Path**: `src/thetis/compiler/` (3 files, ~480 lines)
- **Confidence**: high
- **Covers**: `src/thetis/compiler/core.clj`, `src/thetis/compiler/data.clj`, `src/thetis/compiler/forms.clj`

## Purpose
Pure compiler operations and code generation. The compiler is modeled as a value (map) — `compiler/core.clj` provides stateless transforms, `forms.clj` emits Clojure/CLJS code.

## compiler/core.clj (133 lines) — Pure Operations
Query + update functions on compiler value `{:functions, :types, :guards, :type-state, :guard-state, :expansion}`.
- `add-function`, `extend-function`, `clone-function` — delegate to `functions.registry`
- `register-type-contribution`, `register-guard` — update type/guard state with ns-keyed contributions
- `remove-ns-contributions` — removes all function, type, and guard contributions from a namespace
- `class-extensions` — resolve type keywords to concrete class-level cases

## compiler/data.clj (20 lines) — Constructors
- `make-compiler` — fresh compiler value
- `clj-compiler` / `cljs-compiler` — pre-initialized with platform base types

## compiler/forms.clj (327 lines) — Code Generation
The most complex file. Generates actual Clojure/CLJS code from specs.

Key code-gen functions:
- `declaration` — full `defg` output: clean old defs → defprotocol → defn → register prototypes → extend
- `extension` — `generic+` output: register prototypes → optionally redefine fn (for new guards) → extend
- `function-definition` — emits defn with optional guard two-phase dispatch wrapper
- `protocol-extension` — emits `extend` (CLJ) or prototype assignments (CLJS)
- `thing` — emits `reify` form for anonymous generic implementations
- `cljs-extend1` — CLJS-specific extend via `js*` prototype assignment

Dynamic vars for code-gen rebinding:
- `*prototypes-sym*` — default `thetis.core/prototypes`
- `*guard-impls-sym*` — default `thetis.core/guard-impls`
- `*generic+-sym*` — default `thetis.core/generic+`

## Connections
- Imports: `functions.registry`, `functions.spec`, `types.core`, `types` (user-facing), `utils.*`
- Imported by: `core.clj` (via `compiler.core` and `compiler.forms`)
