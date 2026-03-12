---
type: task
tags: [poly-migration, state-model, compiler-as-value, guards, prototypes, namespaces]
status: done
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T17-29-59-782Z_e1b99b07-852b-4cad-a22c-ad10c694b97e.jsonl
---

# Resolve `:guards`, `:namespaces`, `:prototypes` in the state model

## Context

We're on branch `poly-migration`, migrating nemesis internals to delegate to poly's pure functional layer. The poly compiler is now a fully pure API — all operations are `(compiler, args) → compiler'` with no global state.

The next logical step is making `nemesis.state` hold a poly compiler value instead of its bespoke state map. But there's a blocker: the nemesis state has keys that don't fit the poly compiler model.

### Current nemesis state shape
```clojure
;; nemesis.state/state atom
{:clj  {:fns {}           ;; → maps to poly compiler :functions
         :types {}         ;; → maps to poly compiler :types
         :guards {}        ;; ← doesn't fit
         :namespaces {}    ;; ← doesn't fit
         :prototypes {}}   ;; ← doesn't fit
 :cljs {... same ...}}
```

### Poly compiler shape
```clojure
{:functions {}
 :types {}}
```

The `:fns` and `:types` keys map cleanly. The other three need decisions.

### What we know about each key

**`:guards`** — Used by `nemesis.types/get-guards` and `nemesis.types/get-guard`. Related to predicate compilation (`symbolic-pred`, `compile-pred-map`, `isa` macro). Need to understand: are guards set anywhere? Are they read at macro-expansion time or runtime? Could they live in the type registry itself?

**`:namespaces`** — Appears in the initial state but needs investigation. Grep for usage to understand its role.

**`:prototypes`** — This is the runtime implementation registry. `nemesis.core/prototypes` is a separate atom (`(defonce prototypes (atom {}))`). The `:prototypes` key in the compile-time state may be vestigial or serve a different purpose. Need to check if anything writes to `state[:prototypes]` vs the `nemesis.core/prototypes` atom.

### The mutation surface is tiny
Only 2 places write to nemesis state:
1. `nemesis.impl.registry/register-spec!` → `(state/swap! assoc-in [:fns fullname] spec)`
2. `nemesis.core/register-type` → `(state/swap! update :types ...)`

If the three extra keys are unused or can be relocated, the migration to `{:clj poly-compiler :cljs poly-compiler}` is straightforward.

## Files

Key files to investigate:
- `src/nemesis/state.clj` — the state atom and accessors
- `src/nemesis/types.clj` — `get-guards`, `get-guard`, `symbolic-pred`, `isa` macro
- `src/nemesis/core.clj` — `prototypes` atom, `reset-state!`
- `src/nemesis/impl/forms.clj` — `current-compiler` builds poly compiler from state
- `src/poly/compiler/core.clj` — pure update functions (just added)
- `src/poly/compiler/data.clj` — `make-compiler`, `clj-compiler`, `cljs-compiler`

Related journal/notes:
- `.pi/journal/260221:1942_IMPL_poly-migration-pure-compiler-api.md` — what was done this session
- `.knowledge/notes/expansion-state-keep-as-is.md` — decision on `*expansion-state*`
- `.knowledge/notes/poly-compiler-as-value.md` — compiler-as-value pattern

## Task

1. **Audit `:guards`** — grep for all reads/writes. Determine if guards are compile-time or runtime. Decide: should they be a field in the poly compiler, stay in nemesis-only state, or be derived from the type registry?

2. **Audit `:namespaces`** — same treatment. May be entirely unused.

3. **Audit `:prototypes`** — clarify the relationship between `state[:prototypes]` and `nemesis.core/prototypes`. Determine if the state key is vestigial.

4. **Design the target state shape** — based on findings, decide:
   - (a) Expand poly compiler to include these concepts (if they're general enough)
   - (b) Keep them as nemesis-specific state alongside the compiler: `{:clj {:compiler poly-val :guards {} ...} :cljs ...}`
   - (c) Relocate them entirely (e.g., guards derived from types, prototypes stays as separate atom, namespaces dropped)

5. **If clear path emerges, implement** — migrate `nemesis.state` to hold poly compiler values. Update `current-compiler` to just read from state + assoc expansion context. Update the two mutation points.

### Invariants
- All tests must pass after every change (`nemesis.tries.one` through `four`)
- Poly modules remain pure (no global state)
- `nemesis.core` public API unchanged
- Run tests: `clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (require (quote nemesis.tries.four)) (println "ALL TESTS OK"))'`
