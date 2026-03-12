# Conventions

## Code Style
- **Namespace structure**: `thetis.<module>` with sub-modules as `thetis.<module>.<sub>`
- **Pure/impure split**: Pure functions take compiler value as first arg; only `state.clj` and `core.clj` touch atoms
- **`do :label` blocks**: Sections within files grouped with `(do :section-name ...)` for organization
- **Dynamic vars**: `*expansion-state*` for macro env/form; `*prototypes-sym*`, `*guard-impls-sym*` for code-gen rebinding
- **defmac**: Custom macro-defining macro in `utils/misc.clj` that auto-wraps with expansion state binding + `doall-rec` to avoid lazy-seq escaping dynamic scope

## Naming Conventions
- Protocol prefix: `I<name>` (e.g., `Ig1`, `Ifoo`)
- Method prefix: `p_<name>` (e.g., `p_g1`, `p_foo`)
- Arity suffix: `_<n>` (e.g., `Ig1_2`, `p_g1_2`)
- Fullname: `ns/name` symbol (e.g., `thetis.tries.one/g1`)
- Function-name helpers in `utils/names.clj`

## State Management
- Single global atom `thetis.state/state` holds `{:clj {...} :cljs {...}}` registries
- Compilation target selected by `*expansion-state*` — CLJS detected by `:ns` in `&env`
- Namespace-keyed contributions for incremental builds: clean old contributions, re-register on recompile

## Extension Modes (on generics)
- `:sealed` — no extensions allowed
- `:extend` — new types only, no overrides or specializations
- `:refine` (default) — new types + specializations, no direct overrides
- `:override` — anything goes

## Testing Patterns
- **tries/**: assertion-based integration tests in `.cljc` files, run by requiring namespaces
- **test/poly/**: clojure.test unit tests for pure functions (still use old `poly` namespace — not yet renamed)
- Tests validate idempotent incremental builds extensively

## Anti-patterns to Avoid
- Never use lazy sequences in macro expansion code (will escape `*expansion-state*` binding) — `doall-rec` prevents this
- Never read `*expansion-state*` from pure compiler modules — pass context as data
