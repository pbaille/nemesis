---
title: expansion-state-keep-as-is
created: 2026-02-21
updated: 2026-02-21
type: decision
subsystem: poly
status: active
tags: [state-management, expansion, migration]
related: ["[[poly-compiler-as-value]]", "[[poly-migration-strangler-fig]]"]
---

# Keep `*expansion-state*` as env/form context only

## Decision

`nemesis.state/*expansion-state*` stays as `{:env &env :form &form}` — it does NOT become a poly compiler value.

## Context

During the poly migration (strangler fig), we introduced `current-compiler` in `nemesis.impl.forms` that builds a poly compiler snapshot from nemesis mutable state on each call:

```clojure
(defn current-compiler []
  {:functions (reg/get-reg)
   :types (t/get-reg)
   :expansion {:env (state/env) :form (state/form)}})
```

The question: could `*expansion-state*` hold this compiler value directly, eliminating the need to rebuild it each time?

## Why not

**Macros mutate state during expansion.** `defg` calls `reg/register-spec!` which writes to the state atom. If we snapshotted state into the dynamic var at macro entry (via `defmac`'s `state/expanding`), subsequent reads within the same expansion would see stale data.

The current split is actually correct:
- **`state` atom** — mutable registry of fns, types, guards. Written during macro expansion.
- **`*expansion-state*`** — per-expansion context (env, form). Bound once, read-only.
- **`current-compiler`** — builds a fresh poly-compatible snapshot when poly functions need one. Always reads latest state.

## Alternatives considered

1. **Compiler atom**: Make `current-compiler` a live view (e.g., custom IDeref type). Adds complexity, no clear gain.
2. **Var metadata for specs**: Would complicate the dual-target (:clj/:cljs) mechanism that keys off `state/cljs?`.
3. **Replace state atom with compiler atom**: Would require rethinking all state mutations to go through poly's functional update model. Worth revisiting when/if nemesis.core macros are rewritten.

## Implication

The `defmac` → `state/expanding` → `current-compiler` chain is the intended architecture. `current-compiler` is cheap (two map lookups from an atom) and always fresh. This is fine.
