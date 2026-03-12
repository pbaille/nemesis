---
title: runtime-state-facilities
created: 2026-02-22
updated: 2026-02-22
type: open-thread
subsystem: generics
status: parked
tags: [runtime, prototypes, wrap, middleware, object-construction]
related: ["[[poly-compiler-as-value]]", "[[state-bookkeeping-needs]]"]
---

# Runtime State Facilities

## Idea

Since poly has a runtime `prototypes` atom that holds all implementations:

```clojure
;; prototypes shape:
{type-keyword → generic-name → arity → fn}
```

...we could expose facilities to dynamically wrap, update, or intercept behavior at the generic or implementation level.

### Possible Capabilities

#### 1. Wrap/update at generic level

Intercept all calls to a generic, regardless of type:

```clojure
;; Hypothetical API
(generic-wrap g1
  (fn [original-fn x]
    (println "calling g1 with" x)
    (original-fn x)))
```

This would update every prototype entry for `g1` across all types and arities. Useful for debugging, tracing, memoization.

#### 2. Wrap/update at implementation level

Intercept calls to a specific type's implementation:

```clojure
;; Hypothetical
(impl-wrap g1 :vec
  (fn [original-fn x]
    (count x)))  ;; override vec behavior
```

This is essentially runtime `:override` mode — but reversible, since the original fn is captured in the closure.

#### 3. Extract methods to build objects on the fly

Pull implementations out of the prototype registry to construct ad-hoc objects:

```clojure
;; Hypothetical — extract all :vec implementations of g1
(extract-impl g1 :vec) ;; → {1 (fn [x] ...) 2 (fn [x y] ...)}
```

The `notes.org` caveat: "maybe not possible on JVM" — likely referring to the fact that JVM protocol dispatch goes through interface method calls, not through the prototypes atom. The prototypes atom is the *source* but protocol extension via `extend-type` copies the fn into the protocol's dispatch mechanism.

**On CLJS**: Prototype-based dispatch makes this more natural. The emitted code reads from the prototypes atom at call time, so updating it has immediate effect.

**On CLJ**: Protocol dispatch is compiled into interface method calls. Changing the prototypes atom after `extend-type` has run doesn't change the already-compiled dispatch. You'd need to re-run `extend-type` with the wrapped fn.

## Platform Divergence

This is one area where CLJ and CLJS have fundamentally different capabilities:

- **CLJS**: Prototype lookup at runtime → wrap/update is natural and immediate
- **CLJ**: Protocol dispatch baked into JVM interfaces → need to re-extend after wrapping

A cross-platform API would need to abstract over this difference, possibly by re-emitting `extend-type` calls when prototypes change on CLJ.

## Status: Parked

This is a nice-to-have capability, not blocking any current work. The ideas are interesting but need concrete use cases to drive the design. Wrapping/middleware on generics could be powerful for debugging and testing, but the CLJ/CLJS divergence makes a clean API non-trivial.

## Origin

From `notes.org`:
> Since we've introduced a runtime state that holds implementations at runtime, we could add some facilities to wrap/upd behavior at the generic or implementation level. Extract methods to build object on the fly (maybe not possible on JVM)
