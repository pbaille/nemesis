# Module: state

- **Path**: `src/thetis/state.clj` (131 lines)
- **Confidence**: high
- **Covers**: `src/thetis/state.clj`

## Purpose
Global mutable state for the compile-time registry. Holds both CLJ and CLJS registries side-by-side in a single atom. Manages expansion context (macro `&env`/`&form`).

## Key Abstractions
- `state` atom: `{:clj {:functions {} :types {} :type-state {} :guards {} :guard-state {}} :cljs {...}}`
- `*expansion-state*` dynamic var: `{:env &env :form &form}` — bound by `expanding` macro
- `compilation-target` — returns `:clj` or `:cljs` based on `*expansion-state*`

## Important Functions
- `get` / `get-in` — read current platform's state
- `swap!` — update current platform's state via `(update state target f args)`
- `init-types!` — populates both CLJ/CLJS type registries from `types.data` (called at load time)
- `qualify-symbol` — resolves symbol using expansion env (CLJ `resolve` or `cljs.analyzer/resolve-var`)

## Dead Code — REMOVED (worker-dead-code, 2026-03-13)
Removed: `debug` atom, `lambda-case-compiler*` atom, `get-in` fn, `display` macro, `targeting-cljs` macro. `:refer-clojure :exclude` updated (no longer excludes `get-in`).

## Incremental Build Support
- `prepared-namespaces` atom: tracks which ns have been cleaned in current build pass
- `ns-prepared?` / `mark-ns-prepared!` / `reset-prepared!` — idempotent cleanup per ns per build

## Connections
- Imports: `utils.expansion`, `types.core`, `types.data`
- Imported by: `core.clj`, `types.clj`
