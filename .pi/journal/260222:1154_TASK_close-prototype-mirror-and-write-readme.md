---
type: task
tags: [prototype-mirror, documentation, readme, housekeeping, open-threads]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T08-16-25-227Z_712d50c2-69e1-43ca-9b01-7d00f76b35a2.jsonl
---

# Close the last open thread + write a README

## Context

All major design threads in poly are now resolved and implemented:

- ✅ Extension modes (`:sealed`/`:extend`/`:refine`/`:override`) — `poly.tries.four`
- ✅ Fork semantics — full structural clone, mode reset to `:override`
- ✅ Case accumulation — namespace-keyed, deterministic ordering
- ✅ Predicate guards — `defguard` macro, runtime `guard-impls` atom, two-phase dispatch, extension mode integration. `poly.tries.five`
- ✅ CLJS compatibility — all 5 tries compile and pass under shadow-cljs
- ✅ Incremental builds — `prepare-state!` hook with ns-keyed contributions

The **only remaining open concern** in `[[state-bookkeeping-needs]]` is the **compile-time prototype mirror** — the question of whether the compile-time registry should mirror runtime-only extensions (extensions made outside macros, directly to the `prototypes` atom). The note already asks: "Is runtime-only extension a supported use case? If all mutations go through macros, the compile-time spec is always authoritative and a prototype mirror is unnecessary."

There is **no README** or user-facing documentation for poly. The project has no `doc/` directory. The only documentation is the `notes.org` (archived) and the `.knowledge/` vault (design notes for maintainers).

## Files

### Open thread to close
- `.knowledge/notes/state-bookkeeping-needs.md` — Section 3 (Compile-time Prototype Mirror). Assess and either implement, park, or close.
- `.knowledge/notes/runtime-state-facilities.md` — Related parked thread about runtime wrap/extract. May inform the prototype mirror decision.
- `.knowledge/maps/open-threads-map.md` — Update status once decided.

### Key source files for understanding the runtime model
- `src/poly/core.clj` — `prototypes` atom, `guard-impls` atom, all public macros
- `src/poly/compiler/forms.clj` — `prototype_registering`, `guard_registering`, how runtime state is populated
- `src/poly/state.clj` — compile-time state model

### Test files (document the full API surface)
- `src/poly/tries/one.cljc` — `defg`, `generic+`, `type+`, `thing`, `fork`, `fork+`, `deft`, `register-type`, multi-arity, variadic
- `src/poly/tries/two.cljc` — (check what it covers)
- `src/poly/tries/three.cljc` — (check what it covers)
- `src/poly/tries/four.cljc` — extension modes (`:sealed`/`:extend`/`:refine`/`:override`), fork mode reset
- `src/poly/tries/five.cljc` — predicate guards (`defguard`), guard + extension modes, guard + fork, multi-arity guards

### Project config
- `deps.edn` — dependencies, aliases
- `shadow-cljs.edn` — CLJS build config

## Task

### 1. Close the compile-time prototype mirror thread

Read `state-bookkeeping-needs.md` Section 3 and `runtime-state-facilities.md`. The key question: does poly support runtime-only extension (bypassing macros)?

Evidence to check:
- Is `prototypes` atom ever read during macro expansion? (grep for `@prototypes` or `deref prototypes` in macro code)
- Does `fork` depend on compile-time spec only, or does it also read runtime prototypes?
- Is there any code path where a user could `swap!` prototypes directly?

If the answer is "all mutations go through macros," then the compile-time spec IS the authoritative registry, the prototype mirror is unnecessary, and this thread can be **closed**. Write a short decision note or update the existing note with the conclusion.

If there IS a use case for runtime extension, design what the mirror would look like (but this is unlikely).

### 2. Write a README.md

Create `README.md` at project root. Target audience: Clojure developers who want cross-platform polymorphism.

Structure:
- **What is poly?** — One-paragraph pitch. Type-keyword dispatch, cross-platform (CLJ + CLJS), zero-overhead protocol-based.
- **Quick start** — `deps.edn` dependency, basic `defg`/`generic+` example
- **Core concepts** — Type keywords, type hierarchy (`:coll` → `:map :set :seq :vec`), protocol-per-arity, precedence
- **API reference** — `defg`, `generic+`, `type+`, `thing`, `fork`, `fork+`, `deft`, `register-type`, `defguard`, `implements?`
- **Extension modes** — `:sealed`/`:extend`/`:refine`/`:override` with examples
- **Predicate guards** — `defguard` + usage
- **ClojureScript** — How to set up shadow-cljs, the `prepare-state!` hook
- **Custom types** — `register-type`, `deft`

Use the tries files as source material — they demonstrate every feature with assertions.

### 3. Update vault

- Update `open-threads-map.md` — move prototype mirror to resolved/parked
- Update `state-bookkeeping-needs.md` — mark fully resolved or note what's still open
- Update `goals.md` — reflect completion of open threads, pivot to documentation/packaging
