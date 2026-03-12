# Module: types

- **Path**: `src/thetis/types.clj` (117 lines), `src/thetis/types/` (2 files, ~236 lines)
- **Confidence**: high
- **Covers**: `src/thetis/types.clj`, `src/thetis/types/core.clj`, `src/thetis/types/data.clj`

## Purpose
Type keyword system — maps platform-independent keywords (`:vec`, `:map`, `:coll`) to concrete host classes. Provides hierarchy, predicate compilation, and namespace-keyed contribution tracking.

## types/data.clj (60 lines) — Platform Data
Static definitions:
- `clj-base-types` — `{:vec #{IPersistentVector} :map #{PersistentArrayMap PersistentHashMap} ...}`
- `cljs-base-types` — `{:vec #{PersistentVector Subvec BlackNode ...} :seq #{LazySeq Cons ...} ...}`
- `groups` — `{:coll #{:map :set :seq :vec} :word #{:keyword :string :symbol} :indexed #{:vec :seq} :hashed #{:map :set} :builtin <all base>}`
- `preds-symbols` — `{:vec vector? :map map? ...}` for symbolic predicate compilation

## types/core.clj (176 lines) — Pure Registry
Registry = `{keyword → #{members}}` where members are class symbols or other keywords.
- Hierarchy: `children`, `parents`, `childof`, `parentof`, `all-paths`, `cyclic?`
- `classes` — resolve keyword → concrete class symbols (recursive expansion)
- **Namespace-keyed type state**: `{:base {...} :contributions {"ns" {:tags {...} :groups {...}}}}`
  - `register-type-contribution` / `remove-type-contributions` / `effective-types`
- **Guard state**: same pattern — `register-guard-contribution` / `remove-guard-contributions` / `effective-guards`

## types.clj (117 lines) — User-Facing API
Wraps `types/core.clj` with state lookups. Also provides:
- `symbolic-pred` / `symbolic-pred-body` — compile type keyword to predicate code form
- `compile-pred-map` — all type predicates as a map
- `isa` macro — type predicate test

## Key Design: Why So Many CLJS Types?
`:seq` in CLJS maps to ~20 concrete types (LazySeq, Cons, Range, etc.) vs 1 interface in CLJ (ISeq). This is the core problem thetis solves — one keyword, all platforms.

## Connections
- Imports: `utils.misc`
- Imported by: `state.clj`, `compiler/core.clj`, `compiler/forms.clj`, `functions/spec.clj`
