---
title: poly-compiler-as-value
created: 2026-02-21
updated: 2026-02-21
type: insight
subsystem: poly
status: active
tags: [architecture, compiler, functional-core]
related: ["[[poly-migration-strangler-fig]]"]
---

# The Compiler-as-Value Pattern

## Core Idea

In the poly rewrite, the "compiler" is a plain map:

```clojure
{:functions  fn-registry-map     ;; generic specs by qualified name
 :types      type-registry-map   ;; type keyword → classes/groups
 :expansion  {:env &env :form &form}  ;; macro expansion context
 :class-resolver  (fn [type] [classes...])  ;; optional
}
```

All poly functions take this map as first argument. No global state reads.

## Why This Matters

1. **Testable** — You can construct a compiler value in tests without touching any atoms
2. **Forkable** — Creating an isolated compilation context is just `assoc`
3. **CLJS-ready** — The expansion context makes platform detection explicit, not magic

## Current Bridge

`nemesis.impl.forms/current-compiler` builds this value from nemesis's mutable state:

```clojure
(defn current-compiler []
  {:functions (reg/get-reg)
   :types (t/get-reg)
   :expansion {:env (state/env) :form (state/form)}
   :class-resolver t/classes})
```

## Tension

The compiler-as-value is beautiful, but macros inherently need *somewhere* to stash state between expansions. If `defg` registers a spec, the next `generic+` call needs to find it. You can't just thread the compiler through — macros don't have continuations.

Options:
1. Keep a global compiler atom (nemesis's current approach)
2. Make the compiler a build-system hook (shadow-cljs `:compile-prepare`)
3. Store specs in metadata on the vars they create
