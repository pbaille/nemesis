---
title: state-shape-aligned-with-poly-compiler
created: 2026-02-21
updated: 2026-02-21
type: decision
subsystem: poly
status: active
tags: [state-model, compiler-as-value, poly-migration]
related: ["[[poly-compiler-as-value]]", "[[expansion-state-keep-as-is]]"]
---

# Nemesis state shape now matches poly compiler shape

After auditing the three extra keys in `nemesis.state` (`:guards`, `:namespaces`, `:prototypes`), all were found to be dead code:

- **`:guards`** — `get-guards` and `get-guard` defined in `nemesis.types` but never called by anything. No writes either. Planned feature, never implemented.
- **`:namespaces`** — Defined in initial state, never read, never written. Pure dead weight.
- **`:prototypes`** — Vestigial in compile-time state. The actual prototypes atom is `nemesis.core/prototypes` (a separate `defonce`), referenced at code-gen time via `*prototypes-sym*` dynamic var.

With these removed and `:fns` renamed to `:functions`, the nemesis state shape is:

```clojure
{:clj  {:functions {} :types {}}
 :cljs {:functions {} :types {}}}
```

This matches the poly compiler shape exactly:

```clojure
{:functions {}
 :types {}}
```

## Caveat

These keys were not truly "dead code" — they were **intentional placeholders** for bookkeeping needed to handle extension modes, fork correctness, and CLJS incremental recompilation properly. The underlying design concerns remain open — see [[state-bookkeeping-needs]].

The keys were removed because they had no reads/writes (no implementation yet), and having empty maps in state obscures which fields are actually functional. They should be re-added when concrete implementations materialize.

## Consequence

`current-compiler` simplified from assembling a map from separate reads:

```clojure
;; before
{:functions (reg/get-reg) :types (t/get-reg) :expansion {...}}

;; after
(assoc (state/get) :expansion {:env (state/env) :form (state/form)})
```

This is a stepping stone toward making `nemesis.state` hold actual poly compiler values, where mutations go through `poly.compiler.core` update functions instead of raw `assoc-in`.

## What's NOT changed

- `nemesis.state` still has its own atom with `:clj`/`:cljs` platform keys
- `*expansion-state*` remains a separate dynamic var (per [[expansion-state-keep-as-is]])
- `prototypes` remains a separate runtime atom in `nemesis.core`
- Mutation still happens via `state/swap!` with raw `assoc-in`
