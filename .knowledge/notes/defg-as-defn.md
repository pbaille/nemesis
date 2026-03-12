---
title: defg-as-defn
created: 2026-02-22
updated: 2026-02-22
type: open-thread
subsystem: generics
status: parked
tags: [defg, defn, convention, first-arg, lazy-generic]
related: ["[[three-extension-modes]]"]
---

# defg-as-defn: Lazy Generic Promotion

## Idea

If a generic has only default implementations (no type-specific cases), it's functionally identical to a normal `defn`. The insight: you could define functions with `defg` syntax, and they'd start as plain functions until someone adds a type-specific extension — at which point they become true protocol-backed generics.

```clojure
;; Starts as a plain function
(defg process [x]
  (do-something x))

;; Still just a function — no protocols generated
(process 42) ;; → (do-something 42)

;; NOW it becomes a generic
(generic+ process [x]
  :vec (do-special-vec-thing x))

;; After extension, protocol dispatch kicks in
(process [1 2 3]) ;; → dispatches via protocol
(process 42)      ;; → falls through to default
```

## Why This Is Interesting

### 1. Convention enforcement

This encourages the convention of **object-as-first-argument** for all functions. If every `defg` could potentially become polymorphic, you naturally write functions that take the "thing being operated on" as the first arg.

### 2. Zero-cost default

Until someone actually extends a generic, there's no protocol overhead. No protocol objects generated, no `extend-type` calls emitted. Just a plain function call.

### 3. Library design pattern

A library could define its API as `defg` functions with defaults. Downstream consumers can then specialize behavior for their types without the library author having to anticipate polymorphism upfront.

## Challenges

### Protocol generation timing

Currently `defg` generates protocols at macro expansion time. If we defer protocol generation until the first `generic+` call, we need:

1. A way for `generic+` to retroactively generate protocols for a spec that started as defn-only
2. The spec must be in the compile-time registry from the start (so `generic+` can find it)
3. The emitted code must be different: initially a plain `defn`, later re-emitted as protocol dispatch

This is architecturally complex. The simpler version: **always generate protocols**, but if there are no type-specific cases, the dispatch function just calls the default directly. The protocols exist but are empty. Cost: a few unused protocol definitions.

### Runtime transition

If a function starts as a `defn` and later becomes protocol-dispatched, existing compiled call sites still call the plain function. On CLJ, this might require re-compilation. On CLJS, if the var holds a dispatch fn that reads from prototypes, it could work seamlessly.

### Interaction with extension modes

A defg-as-defn would presumably start with `:mode :extend` by default (you're just providing a default, anyone can add types). Or maybe it needs a new mode: `:open` — explicitly designed to be extended.

## Status: Parked

This is a forward-looking design idea. It doesn't solve any current problem — it's more about what poly's API *could* evolve into. Worth revisiting when the core system is stable and we're thinking about ergonomics.

## Origin

From `notes.org`:
> I could try to completely wrap defn behavior. I mean that if a generic has only default implementations it is just a normal function. It can become a generic if extended. This way we enforce the convention of passing the object as first argument to all functions.
