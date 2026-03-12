---
title: predicate-guards-design
created: 2026-02-22
updated: 2026-02-22
type: decision
subsystem: types
status: active
tags: [predicate-guards, runtime-dispatch, type-system, design]
related: ["[[state-bookkeeping-needs]]", "[[three-extension-modes]]", "[[forking-semantics]]"]
---

# Predicate Guards — Design

> **Naming note**: References to `poly.*` namespaces in this document reflect the pre-rename state. Current namespaces are `thetis.*`.

## The Problem

The type system is class-based: `:vec` → `IPersistentVector`, dispatch goes through JVM/JS protocols. Zero overhead. But there's no way to define predicate-based types like `:positive` (number + pos?) or `:non-empty-vec` (vector + seq). These require runtime checks — you can't resolve `:positive` to a JVM class.

## Core Insight: Two-Phase Dispatch

A generic with predicate-guarded cases uses **two-phase dispatch**:

1. **Phase 1 — Predicate cond-chain** (runtime): Check predicate guards in order. First match wins.
2. **Phase 2 — Protocol dispatch** (compile-time): If no predicate guard matches, fall through to normal protocol-based dispatch.

This means predicate guards are **prepended** to the dispatch path. They're checked first because they're always more specific than class-based types (`:positive` is a subset of `:number`).

### Performance

- Generics with **no predicate guards**: zero change. Protocol dispatch as before.
- Generics with predicate guards: one cond-chain before protocol dispatch. Cost proportional to number of guards. Only generics that opt into guards pay the cost.

## Registration: `defguard`

Predicate types are registered separately from class-based types. They're not part of the type registry (they don't map to classes). Instead, they're a new concept: **guarded types**.

```clojure
(defguard :positive
  :base :number          ;; parent type — must be a registered type keyword
  :pred pos?)            ;; additional predicate (applied after base type check)

(defguard :non-empty-vec
  :base :vec
  :pred seq)

(defguard :non-empty-coll
  :base :coll
  :pred seq)
```

### Why `:base`?

A guard must declare its base type for three reasons:

1. **Efficiency**: The base type check narrows candidates before running the (potentially expensive) predicate. `pos?` on a string would throw — the `:number` base check prevents that.
2. **Hierarchy integration**: `:positive` is a child of `:number`. This matters for extension mode checks (`:extend` blocks specialization from parent, `:refine` allows it).
3. **Precedence**: Guards with the same base type need ordering. Guards with different base types are independent.

### Composite predicates

The `:pred` can be any function. For multi-predicate guards, the user composes:

```clojure
(defguard :small-pos-number
  :base :number
  :pred (fn [x] (and (pos? x) (< x 100))))
```

No special syntax for `and` / `or` — just functions. Keep it simple.

## Storage: Guards on the Compiler Value

Guards live on the compiler value alongside `:types` and `:functions`:

```clojure
{:types {...}
 :functions {...}
 :guards {:positive   {:base :number :pred pos?}
          :non-empty-vec {:base :vec :pred seq}}}
```

The `:guards` map is:
- Compile-time: stores the guard spec (base + pred symbol)
- Runtime: the pred function itself (evaluated once at definition time)

### Namespace-keyed contributions

Like types and functions, guard contributions are keyed by namespace for incremental builds:

```clojure
{:guard-state {:base {}
               :contributions {"my.ns" {:non-empty-vec {:base :vec :pred seq}}}}}
```

`prepare-ns!` cleans guard contributions on recompile, same pattern as types and functions.

## Using Guards in `defg` / `generic+`

Guards are used exactly like type keywords — they're just more specific types:

```clojure
(defguard :positive
  :base :number
  :pred pos?)

(defg describe [x]
  :positive [:pos x]          ;; predicate guard — checked at runtime
  :number   [:num x]          ;; class type — protocol dispatch
  :vec      [:vec x]
  [:default x])
```

The macro detects that `:positive` is a guard (not a class type) and changes code generation for that case.

## Code Generation

### Without guards (status quo)

```clojure
(defg g1 [x]
  :vec [:vec x]
  :number [:num x]
  [:default x])
```

Emits:
- Protocol `G1$1` with method `g1_1`
- `extend-type IPersistentVector G1$1 (g1_1 [x] [:vec x])`
- `extend-type Number G1$1 (g1_1 [x] [:num x])`
- `extend-type Object G1$1 (g1_1 [x] [:default x])`
- `(defn g1 [x] (g1_1 x))` — direct protocol dispatch

### With guards

```clojure
(defg describe [x]
  :positive [:pos x]
  :number   [:num x]
  :vec      [:vec x]
  [:default x])
```

Emits:
- Protocol `Describe$1` with method `describe_1`
- `extend-type Number Describe$1 (describe_1 [x] [:num x])` — the `:number` class case
- `extend-type IPersistentVector Describe$1 ...` — the `:vec` class case
- `extend-type Object Describe$1 ...` — default
- **Changed defn**:

```clojure
(defn describe [x]
  (cond
    ;; predicate guards checked first, in declaration order
    (and (number? x) (pos? x))    [:pos x]
    ;; then fall through to protocol dispatch
    :else (describe_1 x)))
```

The key change: when a generic has guard cases, the `defn` wraps the protocol method call with a cond-chain that checks guards first.

### Guard predicate expansion

The guard check `(and (number? x) (pos? x))` is the **base type predicate** (from `symbolic-pred-body`) followed by the guard's own `:pred`. This reuses the existing predicate compilation machinery in `poly.types`.

```clojure
;; For guard :positive with base :number, pred pos?
;; expands to:
(and (number? x) (pos? x))

;; For guard :non-empty-coll with base :coll, pred seq
;; expands to:
(and (or (map? x) (set? x) (vector? x) (seq? x)) (seq x))
```

### Multi-arity

For multi-arity generics, guards are per-arity. A guard case at arity 2 only affects the arity-2 dispatch:

```clojure
(defg f
  ([x] :positive [:pos x] [:default x])
  ([x y] :positive [:pos x y] [:default x y]))
```

Each arity's `defn` clause gets its own guard cond-chain.

### Variadic arity

Guards on variadic arities work the same — the cond wraps the variadic method call.

## Precedence Rules

### Guards vs class types

Guards are **always checked before** protocol dispatch. Within the guard cond-chain, order follows **declaration order** (same as class-based cases — "like cond, first match wins").

### Multiple guards on the same base type

If both `:positive` (base `:number`) and `:even-number` (base `:number`) exist on the same generic:

```clojure
(defg f [x]
  :positive    [:pos x]
  :even-number [:even x]
  :number      [:num x]
  [:default x])
```

The cond-chain preserves declaration order:
```clojure
(cond
  (and (number? x) (pos? x))    [:pos x]
  (and (number? x) (even? x))   [:even x]
  :else (f_1 x))  ;; falls through to protocol (:number and :default)
```

A value like `2` (positive and even) hits `:positive` first. This is consistent with how class-based dispatch works: `:vec` before `:coll`.

### Guards on overlapping base types

`:positive` (base `:number`) and `:non-empty-vec` (base `:vec`) are independent — the base type check ensures they never conflict.

### Guards on parent/child base types

`:non-empty-coll` (base `:coll`) and `:non-empty-vec` (base `:vec`). The more specific guard (`:non-empty-vec`) should be checked first. Since cases are ordered by declaration, **the user controls precedence** — same as with `:vec` vs `:coll` in class dispatch. If you declare `:non-empty-coll` first, it wins for non-empty vectors.

## Extension Mode Interaction

### `:refine` (default)

Guards interact naturally with refinement. `:positive` is a refinement of `:number`, so:

- If `:number` has an impl, adding `:positive` is **specialization** — allowed under `:refine`
- If `:positive` has an impl, adding `:positive` again is a **direct override** — blocked under `:refine`

The mode check needs to know that `:positive`'s base is `:number`. This is why guards store their `:base` — it feeds into the hierarchy check.

### `:extend`

Adding `:positive` when `:number` has an impl → blocked (same as adding `:vec` when `:coll` covers it). The guard is a "child" of its base type.

### `:sealed`

No extensions, so no guards can be added.

### `:override`

Anything goes, as always.

### Implementation

`check-extend-mode` already uses `types/parents` to detect specialization. For guards, we need to include the guard→base relationship in the hierarchy check. Options:

1. **Register guards in the type registry** as pseudo-types: `:positive #{:number}` — makes `parents` return `(:number)` for `:positive`. Clean but conflates class-types with guard-types.
2. **Separate hierarchy for guards**: `check-extend-mode` queries both the type registry and the guard registry for parent relationships. More explicit.
3. **Guard metadata on the case**: Each case knows if it's a guard case and what its base is. Mode checks use this directly.

**Recommendation: Option 1** (register in type registry). The parent/child relationship is real — `:positive` IS a child of `:number` in the type hierarchy. The only difference is dispatch mechanism (protocol vs predicate). The type registry already handles keyword→keyword relationships (`:coll → #{:map :set :seq :vec}`). Adding `:positive → #{:number}` fits naturally.

The `classes` function would return `nil` for `:positive` (no concrete classes), which is the signal that this is a guard type, not a class type.

## Forking

Fork copies all cases including guard cases. Since guard cases are stored in the same `:cases` / `:declaration-cases` / `:extension-cases` structure, they clone identically. No special handling needed.

The guard registry (`:guards` on the compiler) is global — it doesn't need to be forked per-function. A guard like `:positive` is a type-level concept, not a function-level one.

## Platform Implications (CLJ vs CLJS)

**Good news**: Predicate dispatch is platform-independent. The cond-chain is pure Clojure — `(and (number? x) (pos? x))` works identically on CLJ and CLJS. No reader conditionals needed for guard dispatch code.

**The base type predicate** may differ across platforms (compiled from different type registries), but `symbolic-pred-body` already handles this — it reads from the platform-appropriate registry.

This is actually **simpler** than class-based dispatch for CLJS, where extend-type needs special handling for base types (null, string, number, etc.).

## State Shape Summary

### Compiler value

```clojure
{:types     {...}      ;; includes guard types: {:positive #{:number}}
 :functions {...}
 :guards    {:positive {:base :number :pred pos?}
             :non-empty-vec {:base :vec :pred seq}}
 :type-state {...}
 :guard-state {:base {}
               :contributions {"my.ns" {...}}}}
```

### Case structure

Guard cases look like regular cases but with an extra `:guard` flag:

```clojure
{:type :positive
 :arity 1
 :expr [:pos x]
 :compiled (fn [x] [:pos x])
 :guard true           ;; signals this needs runtime dispatch
 :guard-pred (and (number? x) (pos? x))}  ;; compiled predicate form
```

### Function spec

The spec gains a `:has-guards?` flag (derived, not stored) — computed from the presence of any `:guard true` case. This tells the forms layer to emit the two-phase dispatch defn.

## API Summary

```clojure
;; Define a guard type
(defguard :positive
  :base :number
  :pred pos?)

;; Use it like any type keyword
(defg describe [x]
  :positive [:pos x]
  :number [:num x]
  [:default x])

;; Extend with guards
(generic+ describe [x]
  :non-empty-vec [:nev x])

;; Guards work with all existing forms
(type+ :positive
  (describe [x] [:pos-via-type+ x]))

(fork+ describe2 describe
  [x] :positive [:pos2 x])
```

## Implementation Status: COMPLETE ✅

All steps implemented and tested (CLJ + CLJS):

1. ✅ `defguard` macro in `poly.core` — registers in guard registry (NOT type registry — guards don't map to classes)
2. ✅ `poly.functions.parse` tags guard cases during parsing (looks up guard registry)
3. ✅ `poly.compiler.forms/function-definition` emits two-phase dispatch when guards present
4. ✅ `poly.functions.spec/merge-cases` mode checks handle guard→base hierarchy via `guard-parent-types`
5. ✅ `poly.tries.five` test suite — all tests pass on CLJ and CLJS

### Implementation Details (diverged from design)

**Runtime guard dispatch table** — Instead of inlining guard predicates into the defn's cond-chain (which would make guards non-extensible), we use a runtime atom `poly.core/guard-impls`:

```clojure
;; Shape: {generic-fullname → arity → [{:pred (fn [x] ...) :impl (fn [args] ...)} ...]}
(defonce guard-impls (atom {}))
```

This preserves **open extension** — `generic+` can add guard cases without re-emitting the function definition. The defn reads from the atom at call time. Cost: one atom deref + seq scan per call, only for generics that use guards.

**Re-emit on first guard**: When `generic+` adds the first guard case to a generic that was defined without guards, the function definition is re-emitted to add the guard dispatch wrapper. Subsequent guard additions don't trigger re-emit.

**Guards NOT in type registry**: Originally proposed registering guards as pseudo-types in the type registry. This was abandoned because it creates circular hierarchy issues (`:positive → #{:number}` makes `classes :positive` resolve to Number, which is wrong). Instead, extension mode checks use a separate `guard-parent-types` function that queries both the type hierarchy and the guard registry.

### Files Changed

- `src/poly/core.clj` — `defguard` macro, `guard-impls` atom, guards passed through to parse/extend
- `src/poly/core.cljs` — `guard-impls` atom (CLJS shim)
- `src/poly/state.clj` — guard-state in initial state
- `src/poly/types/core.clj` — guard state management (make/effective/register/remove)
- `src/poly/compiler/core.clj` — register-guard, remove-guard-contributions, remove-ns-contributions updated
- `src/poly/compiler/forms.clj` — guards section (guard_registering), two-phase function-definition, extension redefine
- `src/poly/functions/parse.clj` — guard tagging in compile-cases
- `src/poly/functions/spec.clj` — guard-parent-types, class-extensions excludes guards, prototypes excludes guards
- `src/poly/functions/registry.clj` — guards kwarg threaded through
- `src/poly/tries/five.cljc` — comprehensive test suite
- `shadow-cljs.edn` — five.cljc added to build

## Open Questions (remaining)

1. **Guard on `:default`**: Should `(defguard :truthy :base :default :pred identity)` work? Technically yes — but untested.

2. **Inline guards**: Could support `(defg f [x] (number? pos?) [:pos x] ...)` without a separate `defguard`. Less reusable but more concise. Not critical — `defguard` works well.

3. **Guard removal**: Not implemented. If you don't use a guard, it costs nothing. Low priority.
