# Module: core

- **Path**: `src/thetis/core.clj` (214 lines), `src/thetis/core.cljs` (4 lines)
- **Confidence**: high
- **Covers**: `src/thetis/core.clj`, `src/thetis/core.cljs`

## Purpose
Public macro API — the only namespace users require. All macros delegate to the compiler pipeline: parse → state update → code generation.

## Key Abstractions
- `defg` — define a generic function (generates protocols + defn + extends)
- `generic+` — extend an existing generic with new type cases
- `fork` / `fork+` — clone a generic, optionally extend immediately
- `deft` — define a record type with auto-registration and cast generic
- `thing` — anonymous object implementing generics (like `reify`)
- `type+` — extend multiple generics for one type (like `extend-type`)
- `defguard` — define a predicate guard type for runtime refinement dispatch
- `implements?` — test if a value implements generics

## Runtime State
- `prototypes` atom: `{type → {fullname → {arity → fn}}}` — stores compiled implementations
- `guard-impls` atom: `{fullname → {arity → [{:pred fn :impl fn}]}}` — guard dispatch table

## Build Hooks (shadow-cljs)
- `reset-state!` — full reset, used for clean CLJS builds
- `prepare-state!` — incremental reset, only clears ns-preparation tracker

## Internal Flow
1. `prepare-ns!` — on first macro expansion in a ns, removes old contributions
2. Macro calls `parse/parse` → `state/swap! compiler/add-function` or `extend-function`
3. Reads back enriched spec from state
4. Calls `forms/declaration` or `forms/extension` to emit code

## CLJS Stub
`core.cljs` only declares `prototypes` and `guard-impls` atoms — all macro logic runs on the CLJ side during compilation.

## Connections
- Imports: `state`, `compiler.core`, `compiler.forms`, `functions.parse`, `utils.misc`
- Imported by: user code, `tries/*`
