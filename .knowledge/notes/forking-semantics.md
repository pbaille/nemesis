---
title: forking-semantics
created: 2026-02-22
updated: 2026-02-22
resolved: 2026-02-22
type: decision
subsystem: extension
status: resolved
tags: [forking, clone, runtime-state, hermetic-isolation]
related: ["[[namespace-keyed-spec-registration]]", "[[state-bookkeeping-needs]]", "[[poly-compiler-as-value]]"]
---

# Forking Semantics

## Problem

When you `fork` a generic, what exactly happens? And what *should* happen?

### Current Implementation

`fork` calls `clone` in `poly.functions.spec`, which:

1. Snapshots `effective-cases` from the original spec
2. Stores them as the new spec's `declaration-cases`
3. Marks each case with `:cloned true`
4. Sets `extension-cases` to `{}`
5. Generates fresh protocol names/method names under the new symbol

```clojure
;; poly.functions.spec/clone
(let [all-cases (effective-cases original-spec)
      cases (mapv (fn [c] (assoc c :cloned true)) all-cases)]
  (merge original-spec names
         {:cases cases
          :declaration-cases cases
          :extension-cases {}
          :cloned-from (:fullname original-spec)}))
```

This is a **full structural clone** — the fork gets a frozen copy of all cases at the moment of forking. Later extensions to the original don't affect the fork. The fork can be independently extended.

### What Works

- Basic forking: `(fork two/g1)` → creates local `g1` with same behavior
- Fork + extend: `(fork+ g1-clone one/g1 [x] :symbol "I sym")` → fork with immediate extension
- Fork isolation: extending the fork doesn't affect the parent (tested in `tries/two.cljc`)

### What's Unresolved

#### 1. Runtime state cloning — NOT A PROBLEM

The compile-time spec is cloned, but what about the runtime `prototypes` atom?

```clojure
;; prototypes shape:
{type-keyword → generic-name → arity → fn}
```

When a fork creates fresh protocols, the runtime prototype entries under the original generic name still exist. The fork emits new `prototypes_registering` code that writes under the *new* name. But if someone has captured a reference to the original's runtime dispatch fn, they still dispatch through the old prototype path.

**Resolution**: Not a problem in practice. Fork generates completely new protocol interfaces, so old and new are fully independent at the JVM/JS level. The prototypes atom entries are keyed by the *new* generic name — no collision, no leakage. Each fork is hermetically isolated by construction.

#### 2. Structural sharing vs full clone — LOW PRIORITY, NON-BLOCKING

The current approach copies all cases. An alternative: share the declaration-cases structurally (they're immutable vectors) and only diverge on extension-cases. Since `effective-cases` already merges them, this could work.

With the namespace-keyed model, declaration-cases are truly immutable after creation, so structural sharing would be safe.

**Resolution**: Non-blocking optimization. Clojure persistent data structures already share structure internally. The explicit copy is cheap and correct. If profiling ever shows this matters (unlikely — these are small vectors at compile time), it can be revisited. Not worth tracking as an open thread.

#### 3. Fork of a fork — ALREADY WORKS

What happens when you fork a fork? The `effective-cases` of the first fork (which include its extensions) become the `declaration-cases` of the second fork. The `:cloned-from` chain could get long.

**Resolution**: Already works and is tested. `tries/three.cljc` forks `two/g1` which is itself a fork of `one/g1`. `:cloned-from` stores the immediate parent's fullname, which suffices for debugging. Full lineage tracking is not needed for correctness — each fork is self-contained by construction.

#### 4. Fork + extension modes — RESOLVED

~~If the original has `:mode :extend`, does the fork inherit that constraint?~~

**Decision**: Fork always resets to `:mode :override` (most permissive). Implemented in `poly.functions.spec/clone` which adds `:mode :override` to the merge map. Rationale: the point of forking is to get an independent copy you can modify. If someone wants a constrained fork, they can declare it explicitly.

Tested in `poly.tries.four` — forking a `:sealed` generic produces an `:override` fork that can be freely extended and overridden.

## Resolution Summary

All sub-questions have been assessed and closed:

1. **Runtime state cloning** — Non-issue. Fork generates new protocols; prototypes atom entries are keyed by new name. Full isolation by construction.
2. **Structural sharing** — Low-priority optimization, not worth tracking. Clojure persistent structures already share internally.
3. **Fork of a fork** — Already works and is tested in `tries/three.cljc`. `:cloned-from` tracks immediate parent.
4. **Fork + extension modes** — ✅ Resolved. Fork always resets to `:override`. Tested in `tries/four.cljc`.

The full-clone approach is correct, simple, and hermetic. No remaining design questions.

## Origin

From `notes.org`:
> how to represent a forked spec in the compile time registry?
> is an alias enough? No we have to clone in order to be hermetic to further changes.
> runtime state has to be cloned to
