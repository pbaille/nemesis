---
type: task
tags: [extension-modes, rename, refine, override, default-mode, fork-mode-reset]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T07-02-28-265Z_a5c18a7c-fd9c-48e3-aa5b-20b6705a0940.jsonl
---

# Rename extension modes + make `:refine` the default + fork mode reset

## Context

Extension modes control what `generic+` is allowed to do. Four modes are implemented and tested (`:sealed`, `:extend`, `:tune`, `:patch`). After design review, two naming changes and a default change were decided:

### Renames
- `:tune` → `:refine` — "refine" describes what the mode does (specialize subtypes) better than "tune"
- `:patch` → `:override` — "override" directly names the permission it grants (replace existing implementations)

### New default
- Currently `:patch` (now `:override`) is the default — no checks at all
- Change to `:refine` as default — you can add new types and specialize subtypes, but not replace existing direct implementations
- Rationale: poly users already opted into controlled polymorphism; the default should be safe, with `:override` as explicit opt-in

### Fork mode reset
- `spec/clone` currently inherits `:mode` from the parent via `merge`
- Fork should always reset to `:override` — the point of forking is to get an independent copy you can modify
- If someone wants a constrained fork, they can declare it explicitly

### Final mode spectrum
```
:sealed ── :extend ── :refine ── :override
  │            │          │          │
no ext    new types   + specialize  anything
          only        subtypes      goes
                      (DEFAULT)
```

## Files

### Core implementation
- `src/poly/functions/spec.clj` — `check-extend-mode`, `check-tune-mode` (rename to `check-refine-mode`), `merge-cases` (mode dispatch), `clone` (add mode reset)
  - Line 61: `check-tune-mode` → rename fn + docstring + error messages
  - Line 91-96: `case` dispatch — rename `:tune` → `:refine`, `nil` fallback → `:refine` behavior, add `:override` as explicit no-op
  - Line ~126: `clone` — add `:mode :override` to the merge map

- `src/poly/functions/parse.clj` — Line 93: `(or (:mode name-meta) :patch)` → change default to `:refine`

- `src/poly/compiler/core.clj` — no changes needed (just passes through)
- `src/poly/functions/registry.clj` — no changes needed

### Tests
- `src/poly/tries/four.cljc` — rename all `:tune`/`:patch` references, update test descriptions
- `src/poly/tries/one.cljc` — `g2` needs `^{:mode :override}` because it overrides `:vec` at arity 2 via `generic+`
- `src/poly/tries/three.cljc` — `g1` fork overrides `:string`; should be fine if fork resets to `:override`

### Vault notes
- `.knowledge/notes/three-extension-modes.md` — update mode names throughout
- `.knowledge/maps/open-threads-map.md` — update references

## Task

### 1. Rename in `poly.functions.spec`

**a)** Rename `check-tune-mode` → `check-refine-mode`. Update its docstring and error messages to use `:refine` and `:override` instead of `:tune` and `:patch`.

**b)** Update `check-extend-mode` error messages: `:tune` → `:refine`, `:patch` → `:override`.

**c)** Update `merge-cases` mode dispatch:
```clojure
(case (:mode original-spec)
  :sealed   (throw ...)
  :extend   (check-extend-mode ...)
  :refine   (check-refine-mode ...)
  :override nil
  ;; default (nil / unset) → same as :refine
  (check-refine-mode original-spec extension-spec type-registry))
```

Note: the `case` fallthrough (last clause without a test) handles `nil` (no mode declared) and any unknown mode by defaulting to `:refine` behavior.

**d)** In `clone`, add mode reset:
```clojure
(merge original-spec
       names
       {:arities arities
        :cases cases
        :declaration-cases cases
        :extension-cases {}
        :cloned-from (:fullname original-spec)
        :mode :override})
```

### 2. Change default in `poly.functions.parse`

Line 93: `(or (:mode name-meta) :patch)` → `(:mode name-meta)` — let nil flow through (merge-cases handles nil as `:refine`). OR change to `(or (:mode name-meta) :refine)` for explicit storage. The explicit approach is better — the spec should self-document its mode.

### 3. Fix tries that do direct overrides

**tries/one.cljc**: `g2` definition (around line 65) does `(generic+ g2 ([a b] :vec ...))` which overrides the existing `:vec` at arity 2. Add `^{:mode :override}` to the `defg g2` declaration.

**tries/three.cljc**: `(generic+ g1 [x] :string "overstr")` — this is on a fork of `two/g1`. Since fork now resets to `:override`, this should work without changes. Verify.

**tries/two.cljc**: `(generic+ g1 [x] :symbol "I sym 2")` — this is on a fork of `one/g1`. The original has `#{:keyword :symbol}` not `:symbol`, so `[:symbol 1]` is a new pair. Should work under `:refine`. Verify.

**tries/four.cljc**: Update all test code:
- `:tune` → `:refine` in all `defg` metadata, comments, and regex assertions
- `:patch` → `:override` in comments and the `patchable-fn` test
- `patchable-fn` needs `^{:mode :override}` explicitly (it was relying on `:patch` being the default)
- Add a test for default mode (no metadata) behaving as `:refine`
- Add a test for fork mode reset to `:override`

### 4. Run all tests

```bash
clj -M -e "(require 'poly.tries.one)"
clj -M -e "(require 'poly.tries.two)"
clj -M -e "(require 'poly.tries.three)"
clj -M -e "(require 'poly.tries.four)"
npx shadow-cljs compile main
```

All must pass.

### 5. Update vault notes

- `.knowledge/notes/three-extension-modes.md` — rename modes, update the table and all references
- `.knowledge/maps/open-threads-map.md` — update wording
- `.knowledge/notes/forking-semantics.md` — add note that fork resets to `:override`, mark mode inheritance sub-question as resolved

## Expected outcome

- `:tune` → `:refine` everywhere
- `:patch` → `:override` everywhere
- Default mode is `:refine` (new types + specialization OK, direct overrides blocked)
- Fork always resets to `:override`
- All CLJ tries pass
- CLJS shadow-cljs compiles clean
- Vault notes updated
