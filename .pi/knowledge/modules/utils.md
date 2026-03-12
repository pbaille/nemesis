# Module: utils

- **Path**: `src/thetis/utils/` (3 files, ~245 lines)
- **Confidence**: high
- **Covers**: `src/thetis/utils/misc.clj`, `src/thetis/utils/names.clj`, `src/thetis/utils/expansion.clj`

## Purpose
Shared utilities: symbol manipulation, macro helpers, naming conventions, and expansion context detection.

## utils/misc.clj (196 lines)
Largest utility file. Key items:
- `defmac` — macro-defining macro that wraps body with `thetis.state/expanding` (binds `&env`/`&form` to dynamic var) + `doall-rec` (forces lazy seqs within dynamic scope). Also generates companion `-fn` function and `-fn*` partial for programmatic use.
- `doall-rec` — deep eager realization of all nested sequences (critical for macro correctness)
- Symbol utils: `sym`, `fullname`, `with-ns`, `ns-sym`
- Argv utils: `argv_litt`, `argv_variadic?`, `argv_variadify`, `argv_unvariadify`
- `fn-cases_normalize` — normalizes fn case syntax to `[[pattern body] ...]`
- `binding-pattern_ensure-top-level-sym` — ensures destructuring patterns have `:as` binding
- `cljs_prototype-assoc-form` — CLJS JS interop form for prototype assignment

## utils/names.clj (24 lines)
- `name_derive` — from symbol `foo` → `{:ns, :name, :protocol-prefix Ifoo, :method-prefix p_foo, :fullname ns/foo}`
- `name_arify` — append arity suffix: `Ifoo` + 2 → `Ifoo_2`

## utils/expansion.clj (25 lines)
- `cljs?` — detects CLJS target by checking `:ns` in expansion env
- `qualify-symbol` — resolves symbol via `cljs.analyzer/resolve-var` (CLJS) or `resolve` (CLJ)

## Connections
- Imported by: nearly every other module
