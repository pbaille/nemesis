---
type: task
tags: [predicate-guards, type-system, design, open-thread]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T08-11-14-569Z_e70802f5-7a6f-44cb-8081-369ac3a45ed4.jsonl
---

# Design predicate guards for non-class dispatch

## Context

All major open design threads are now resolved except two:

1. **Predicate guards** (from `[[state-bookkeeping-needs]]`) — the only truly *open* design question
2. **Poly compiler as value** — ongoing architectural pattern, not blocking

The vault is clean: forking-semantics is resolved, extension modes are done (`:sealed`/`:extend`/`:refine`/`:override`), case accumulation is settled, CLJS works. `goals.md` points here as the explicit next step.

### The Problem

Poly's type system is class-based. `:vec` maps to `IPersistentVector`, and dispatch goes through JVM/JS protocols — zero overhead. But there's no way to define predicate-based types like `:positive` (= `(and (number? x) (pos? x))`) or `:non-empty-vec` (= `(and (vector? x) (seq x))`).

### Existing Machinery

`src/poly/types.clj` already has predicate compilation infrastructure:

- `symbolic-pred-body` — generates predicate code from the type registry (class → `instance?`, aggregate → recursive `or`)
- `symbolic-pred` / `compile-pred-map` — builds `{type-keyword → (fn [x] ...)}` maps
- `isa` macro — type-checks a value against a type keyword using compiled predicates

This handles class-based types. The open question is extending it to user-defined predicates.

### The Core Tension

Predicate guards require **runtime dispatch** — you can't resolve `:positive` to a JVM class at compile time. This conflicts with protocol-based dispatch, which is poly's performance story. Two approaches:

1. **Cond-chain before protocol dispatch** — check predicates first, fall through to protocol dispatch for class types. Mixes dispatch strategies. Performance cost only for generics that use predicate types.
2. **Separate slow path** — predicate types use a different dispatch mechanism entirely. Clean separation but two code paths to maintain.

### Design Questions to Explore

1. **Registration**: How does a user define a predicate type? `(type+ :positive number? pos?)` ? A new form? Does it go in the type registry alongside class types?
2. **Precedence**: If `:number` and `:positive` both match, which wins? Predicate types are always more specific than their class parent — but what about two overlapping predicates?
3. **Interaction with extension modes**: Does `:refine` allow specializing from `:number` to `:positive`? (Probably yes — it's a subtype.)
4. **CLJS**: Predicate dispatch is platform-independent (just functions), which is actually simpler than class-based dispatch for CLJS.
5. **Forking**: Fork copies cases. Predicate-guarded cases should clone identically — no special handling needed if predicates are just stored as data in the case map.
6. **State shape**: Where do guards live? In the type registry? In the function spec? In a separate `:guards` map on the compiler value?

## Files

- `.knowledge/notes/state-bookkeeping-needs.md` — the open thread documenting this concern (section 2: Predicate Guards)
- `src/poly/types.clj` — existing predicate compilation machinery (`symbolic-pred`, `compile-pred-map`, `isa`)
- `src/poly/types/` — type registry internals (core.clj, data.clj)
- `src/poly/functions/spec.clj` — function spec (where cases and dispatch info live)
- `src/poly/compiler/` — compiler model (where guards would need to integrate)
- `src/poly/core.clj` — public API macros (would need new form or extension to `type+`)
- `.knowledge/notes/runtime-state-facilities.md` — related parked thread about runtime prototypes atom
- `.knowledge/maps/open-threads-map.md` — update when design crystallizes

## Task

### 1. Survey the existing predicate and dispatch machinery

- Read `src/poly/types.clj` (predicate compilation), `src/poly/types/core.clj` and `src/poly/types/data.clj` (registry structure)
- Read `src/poly/functions/spec.clj` (case structure, how types are stored in cases)
- Read `src/poly/compiler/core.clj` (how the compiler value is built, where guards could live)
- Understand how `defg` macro expansion currently generates dispatch code — trace from `src/poly/core.clj` through to emitted `extend-type` calls

### 2. Design the predicate guard system

Work through the design questions above. For each, consider:
- What's the simplest thing that could work?
- What does it cost at compile time vs runtime?
- Does it break any existing invariant?

Produce a design note (`.knowledge/notes/predicate-guards-design.md`) with:
- Proposed registration syntax
- Proposed type registry shape changes
- Proposed dispatch strategy (cond-chain, separate path, or hybrid)
- Precedence rules
- Interaction with extension modes and forking
- Platform implications (CLJ vs CLJS)

### 3. If design is clear enough, prototype

- A minimal `type+` extension or new form that registers a predicate type
- A `defg` that uses it — showing what the macro expansion would look like
- Don't wire it into the full system yet — just validate the design with concrete code

### 4. Update vault

- Create the design note
- Update `[[state-bookkeeping-needs]]` to reflect progress on the predicate guards section
- Update `[[open-threads-map]]` if the thread status changes
