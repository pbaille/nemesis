---
title: three-extension-modes
created: 2026-02-22
updated: 2026-02-22
type: decision
subsystem: extension
status: resolved
tags: [extension-modes, safety, extend, refine, override, sealed]
related: ["[[forking-semantics]]", "[[state-bookkeeping-needs]]"]
---

# Four Extension Modes: sealed / extend / refine / override

## Design

Generic functions declare an extension mode that controls what `generic+` is allowed to do:

```
:sealed ── :extend ── :refine ── :override
  │            │          │          │
no ext    new types   + specialize  anything
          only        subtypes      goes
                      (DEFAULT)
```

| Mode | Behavior | Safety |
|------|----------|--------|
| `:sealed` | No extensions at all — `generic+` throws immediately | Maximum lockdown |
| `:extend` | Only add implementations for new types. No overriding, no specialization. | Safest extensible — existing behavior guaranteed stable |
| `:refine` | Like extend, plus can specialize subtypes (if `:coll` implements g, can add `:vec` impl) | **Default** — medium safety, can change behavior for specific types |
| `:override` | Full override — any implementation can be replaced | Least safe — anything goes, opt-in |

## Default Mode

**`:refine` is the default.** When no mode metadata is given, a generic gets `:refine`. This means:
- You can add new types freely
- You can specialize (add `:vec` when `:coll` covers it)
- You cannot directly replace an existing implementation

To allow full overrides, declare explicitly: `(defg ^{:mode :override} my-fn ...)`

## Implementation

All four modes are enforced at compile time in `poly.functions.spec/merge-cases`:

### `:extend` — strictest extensible mode
- Blocks direct overrides (re-implementing `:vec` when `:vec` exists)
- **Also blocks specialization** (adding `:vec` when `:coll` covers it) — uses type hierarchy via `poly.types.core/parents`
- Error message directs users to `:refine` or `:override`

### `:refine` — default mode
- Blocks direct overrides (same check as `:extend` for exact type+arity overlap)
- **Allows specialization** — adding `:vec` when only `:coll` has an impl is fine (`:vec` is a new `[type arity]` pair in `effective-cases`)
- Error message directs users to `:override`

### `:sealed` — no extensions at all
- Any `generic+` call throws immediately via `ex-info`

### `:override` — no checks
- The `case` dispatch falls through with `nil` (no-op)

### How the type hierarchy check works

The key insight: specialization (adding `:vec` when `:coll` covers it) is already a "new" `[type arity]` pair in `effective-cases` because cases store their exact type keyword. So `:refine` just checks for exact `[type arity]` overlap.

`:extend` is the one that needs extra logic: it additionally checks if the new type is a **child** of an existing type using `types/parents`, and blocks that too.

Type registry is threaded through: `compiler/extend-function` → `fn.reg/extend-spec` → `fn.spec/merge-cases` via `:type-registry` kwarg.

### Nil mode fallback

The `case` dispatch in `merge-cases` handles `nil` (no mode set on spec) by falling through to `check-refine-mode`. This means even if somehow a spec doesn't have a `:mode` key, it behaves as `:refine`.

## Fork Mode Reset

When a generic is forked via `fork`/`fork+`, the clone **always resets to `:override`** regardless of the parent's mode. Rationale: the point of forking is to get an independent copy you can modify freely. If someone wants a constrained fork, they can declare it explicitly after forking.

This is implemented in `poly.functions.spec/clone` which adds `:mode :override` to the merge map.

## Usage

```clojure
;; Default — :refine (specialization OK, overrides blocked)
(defg my-fn [x]
  :coll [:coll x]
  [:default x])

;; Locked down — only new types
(defg ^{:mode :extend} strict-fn [x]
  :vec [:vec x])

;; Wide open — anything goes
(defg ^{:mode :override} flexible-fn [x]
  :vec [:vec x])

;; No extensions at all
(defg ^{:mode :sealed} frozen-fn [x]
  :vec [:vec x])
```

Tested in `poly.tries.four`.

## Design Rationale

From `notes.org`:
> extend and tune modes are just doing extra checks over the most permissive mode patch

The modes form a spectrum of permission. `:override` is the base behavior (no checks). Each safer mode adds constraints. The default should protect users from accidental breakage while still allowing useful specialization — hence `:refine`.

## History

- **Names evolved**: Originally `extend/tune/patch`. Renamed to `extend/refine/override` for clarity — "refine" describes the action (specializing), "override" names the permission it grants.
- **Default changed**: Originally `:patch` (now `:override`) was the default. Changed to `:refine` — poly users already opted into controlled polymorphism; the default should be safe.
