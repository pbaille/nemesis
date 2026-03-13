# Module: functions

- **Path**: `src/thetis/functions/` (3 files, ~401 lines)
- **Confidence**: high
- **Covers**: `src/thetis/functions/parse.clj`, `src/thetis/functions/registry.clj`, `src/thetis/functions/spec.clj`

## Purpose
Parse, store, and manipulate generic function specifications. This is the data model layer for generics.

## functions/parse.clj (153 lines) — Syntax Parser
Transforms `defg`/`generic+` macro forms into structured spec maps.
- `parse` — main entry: extracts name, mode metadata, arities, cases
- `arity` — single arity clause → `{:arity, :argv, :variadic, :cases, :default}`
- `compile-cases` — annotates guard cases, applies lambda-case-compiler transform
- Output spec shape: `{:name, :fullname, :ns, :mode, :arities {n → info}, :cases [{:type, :expr, :arity, :compiled}]}`

## functions/registry.clj (65 lines) — CRUD
Map of `{fullname → spec}`. Operations:
- `add-spec` — register (calls `init-spec`)
- `extend-spec` — extend with mode validation (delegates to `spec/merge-cases`)
- `clone-spec` — clone under new name
- `remove-ns-contributions` — removes specs defined in ns, removes extension entries keyed by ns

## functions/spec.clj (183 lines) — Spec Logic
Core spec manipulation with structured case storage:
- **Declaration vs extension cases**: `{:declaration-cases [...] :extension-cases {"ns" [...]}}`
- `effective-cases` — computes flat case list: extensions first, declaration last. Ordering is reverse-alphabetical by namespace key (deterministic, rebuild-safe). Docstrings corrected by scout-ordering.
- `merge-cases` — merges extension cases with **mode enforcement**:
  - `:sealed` → throws
  - `:extend` → blocks overrides AND specializations (strictest)
  - `:refine` → allows specializations, blocks direct overrides (default)
  - `:override` → allows anything
- `clone` — snapshots effective cases into new declaration-cases (full isolation)
- `class-extensions` — expands type keywords to concrete class-level cases (excludes guards)

## Connections
- Imports: `utils.misc`, `utils.names`, `types.core`
- Imported by: `compiler/core.clj`, `compiler/forms.clj`, `functions/registry.clj`
