---
type: impl
tags: [poly-migration, strangler-fig, compiler-as-value, extension-modes, pure-functional]
status: active
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T17-29-59-782Z_e1b99b07-852b-4cad-a22c-ad10c694b97e.jsonl
---

# Poly migration: steps 1–4 + pure compiler API

Continued the strangler-fig migration on `poly-migration` branch. Completed all 4 steps from the task brief (`260221:1826_TASK_poly-migration-next-steps.md`), then went further to formalize poly as a fully pure functional API.

## Steps completed

### Step 1: Unify `:default`/`:nil` handling
- `nemesis.types/classes` now delegates entirely to `poly.types.core/classes` (3 lines replacing 12)
- Removed hardcoded `:default` and `:nil` branches — the registry already had both via `poly.types.data`
- `nemesis.impl.registry/class-extensions` passes `(t/get-reg)` map instead of `t/classes` fn
- Removed dead `:class-resolver` key from `current-compiler`
- **Type resolution is fully registry-driven now** — no platform-specific branching in nemesis

### Step 2: Delegate `thing_parse-impl-cases`
- `nemesis.impl.forms/thing_parse-impl-cases` → `poly.compiler.forms/thing_parse-impl-cases`
- Key detail: qualifies generic name via `state/qualify-symbol` before passing to poly (user code has unqualified names, poly expects qualified)
- Removed ~20 lines of duplicated logic

### Step 3: `*expansion-state*` decision
- Evaluated making `*expansion-state*` hold a poly compiler value
- **Decision: keep as-is** — macros mutate state during expansion, snapshotting would create stale reads
- The `defmac` → `state/expanding` → `current-compiler` chain is correct
- Documented in `.knowledge/notes/expansion-state-keep-as-is.md`

### Step 4: Extension modes (`:extend`)
- Added `:mode` field to generic specs via symbol metadata: `(defg ^{:mode :extend} my-fn ...)`
- Default mode `:patch` preserves current behavior (full override)
- `:extend` mode blocks overriding existing type/arity implementations
- Check lives in `poly.functions.spec/merge-cases` (pure, no state)
- Test file: `src/nemesis/tries/four.cljc`

## Pure compiler API (beyond the task brief)

Pierre wanted poly to be fully data-driven/stateless. Added compiler-level update functions to `poly.compiler.core`:

| Function | Signature | Purpose |
|---|---|---|
| `add-function` | `(compiler, spec) → compiler'` | Register a generic |
| `extend-function` | `(compiler, ext-spec) → compiler'` | Extend with new cases |
| `clone-function` | `(compiler, name, new-name) → compiler'` | Clone a generic |
| `add-type` | `(compiler, tag, members) → compiler'` | Register a type |
| `remove-type` | `(compiler, tag) → compiler'` | Remove a type |
| `update-types` | `(compiler, f, args...) → compiler'` | Arbitrary type transform |
| `implementers-map` | `(compiler) → {name → types}` | Query implementing types |

Also added to `poly.compiler.data`:
- `make-compiler` — factory for fresh compiler values
- `clj-compiler` / `cljs-compiler` — pre-loaded with platform base types + groups

### Bug fixes uncovered
- `poly.functions.registry/add-spec` used `assoc-in` on a flat map (latent bug, never hit because nemesis bypassed it for atom writes). Fixed to `assoc`.
- `poly.types.core/add-type` assertion rejected class symbols as members. Fixed to only validate keyword members.

## What's next

The natural follow-up: **make `nemesis.state` hold a poly compiler value** instead of its bespoke `{:fns :types :guards :namespaces :prototypes}` map. The mutation surface is tiny (2 write points). Open question: what to do with `:guards`, `:namespaces`, `:prototypes` which don't fit the poly compiler model.

## Key files modified
- `src/poly/compiler/core.clj` — update functions added
- `src/poly/compiler/data.clj` — compiler factories
- `src/poly/functions/registry.clj` — `assoc-in` → `assoc` fix
- `src/poly/functions/spec.clj` — extension mode check
- `src/poly/functions/parse.clj` — `:mode` extraction from metadata
- `src/poly/types/core.clj` — `add-type` assertion fix
- `src/nemesis/types.clj` — `classes` delegation
- `src/nemesis/impl/forms.clj` — `thing_parse-impl-cases` delegation, `current-compiler` cleanup
- `src/nemesis/impl/registry.clj` — pass registry map not fn
- `src/nemesis/tries/four.cljc` — extension mode tests
- `.knowledge/notes/expansion-state-keep-as-is.md` — decision note
