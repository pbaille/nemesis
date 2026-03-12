---
type: task
tags: [extension-modes, tune, type-hierarchy, spec, compile-time-check]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T06-55-45-673Z_b24ba9a4-c1f2-400f-92ee-442170ada632.jsonl
---

# Implement `:tune` extension mode

## Context

Poly generics support extension modes declared via metadata: `(defg ^{:mode :extend} my-fn ...)`. Currently two modes work:

- **`:patch`** (default) — full override, no checks
- **`:extend`** — only new type+arity pairs allowed, compile-time assertion on overlap

The third mode, **`:tune`**, is designed but not implemented. It's referenced in error messages ("Use :tune or :patch mode to allow overrides") but has no enforcement logic.

`:tune` allows **subtype specialization**: if `:coll` (a group containing `:vec`, `:map`, `:set`, `:seq`) has an implementation, you can add a `:vec`-specific implementation. This is not an "override" — it's a refinement. But you still can't replace an existing direct implementation (e.g., if `:vec` already has its own case, you can't override it in `:tune` mode).

Additionally, `:sealed` mode (no `generic+` at all) is a natural companion — trivial to implement once the mode dispatch is in place.

### Vault notes
- `.knowledge/notes/three-extension-modes.md` — full design analysis with implementation sketch
- `.knowledge/notes/forking-semantics.md` — fork + mode inheritance question (secondary concern)
- `.knowledge/maps/open-threads-map.md` — tracks this as active

## Files

### Core implementation
- `src/poly/functions/spec.clj` — `check-extend-mode` (line ~36), `merge-cases` (line ~47). This is where `:tune` check goes.
- `src/poly/types/core.clj` — `parents` fn (line ~51) provides type hierarchy queries needed by `:tune`.
- `src/poly/functions/registry.clj` — `extend-spec` calls `merge-cases`. May need to thread type registry through.

### State / macro layer
- `src/poly/state.clj` — holds the type registry; `merge-cases` needs access to it for hierarchy queries
- `src/poly/core.clj` — macro layer that calls `extend-spec!`; may need to pass type registry

### Tests
- `src/poly/tries/four.cljc` — existing extension mode tests (`:extend` + `:patch`)

## Task

### 1. Implement `check-tune-mode` in `poly.functions.spec`

Add a `check-tune-mode` function alongside `check-extend-mode`. The logic:

```
For each case in the extension:
  If the [type, arity] pair already exists in effective-cases:
    Check if there's an ANCESTOR type (group) that covers this type at this arity
    If yes → this is specialization → OK
    If no → this is a direct override → THROW
  If the [type, arity] is new → OK (same as :extend)
```

This needs the type registry to call `poly.types.core/parents`. The signature becomes:
```clojure
(defn- check-tune-mode [original-spec extension-spec type-registry]
  ...)
```

**Key question**: `parents` returns the parent *type keywords* (e.g., `:coll` is a parent of `:vec`). But cases store `:type` as a keyword too. So the check is: "does any existing case at this arity have a `:type` that is a parent of the new case's `:type`?"

Use `poly.types.core/parents` which takes `[reg x]` and returns parent type keywords transitively.

### 2. Thread type registry through `merge-cases`

Currently `merge-cases` doesn't receive the type registry. Options:

**Option A** — Add optional `type-registry` parameter to `merge-cases`:
```clojure
(defn merge-cases [original-spec extension-spec extension-ns
                   & {:keys [allow-default-overides type-registry]}]
  ...
  (case (:mode original-spec)
    :extend (check-extend-mode original-spec extension-spec)
    :tune   (check-tune-mode original-spec extension-spec type-registry)
    :sealed (throw ...)
    nil))   ;; :patch or unset — no check
```

**Option B** — Always pass the compiler value (which contains both spec and types). This is more aligned with the poly compiler-as-value pattern but requires more plumbing.

Option A is simpler and sufficient. The type registry is only needed for `:tune`, so it's fine as an optional kwarg.

Then update `poly.functions.registry/extend-spec` to accept and forward the type registry:
```clojure
(defn extend-spec [reg extension-spec extension-ns & {:keys [type-registry]}]
  (add-spec reg
            (spec/merge-cases (get! reg (:fullname extension-spec))
                              extension-spec
                              extension-ns
                              :type-registry type-registry)))
```

And update the call site in `poly.state` or `poly.core` where `extend-spec!` is invoked to pass the current type registry.

### 3. Implement `:sealed` mode

Trivial: in `merge-cases`, before any case-level checks:
```clojure
(when (= :sealed (:mode original-spec))
  (throw (ex-info (str "Generic " (:fullname original-spec) " is sealed — no extensions allowed.")
                  {:generic (:fullname original-spec)})))
```

### 4. Add tests in `tries/four.cljc`

Test cases needed:

```clojure
;; :tune mode — specialization allowed
(defg ^{:mode :tune} tunable-fn [x]
  :coll [:tune-coll x]
  [:tune-default x])

;; ✅ OK — :vec is a child of :coll, this is specialization
(generic+ tunable-fn [x] :vec [:tune-vec x])

;; ❌ THROWS — :coll already has direct impl, can't override
;; (generic+ tunable-fn [x] :coll [:override-coll x])

;; ❌ THROWS — :number has no parent with an impl
;; (generic+ tunable-fn [x] :number [:override-num x])
;; Wait — :number is NEW, not an override. :tune allows new types.
;; Only direct re-implementation of an already-covered type should fail.

;; :sealed mode
(defg ^{:mode :sealed} locked-fn [x]
  :vec [:locked-vec x]
  [:locked-default x])

;; ❌ THROWS — sealed, no extensions at all
;; (generic+ locked-fn [x] :map [:locked-map x])
```

Use the same `macroexpand`-in-try pattern from the existing `:extend` tests for compile-time assertion checking.

### 5. Consider: fork mode inheritance

While touching mode logic, decide (or at least document the decision) whether `clone` in `poly.functions.spec` should preserve or reset the `:mode` field. Current behavior: preserves (fork inherits parent mode). Recommended: reset to `:patch` unless explicitly specified. This is a small change in `clone`:

```clojure
;; In clone, after merge:
(dissoc result :mode)  ;; or (assoc result :mode :patch)
```

This is optional — can be deferred if it needs more thought.

### 6. Update vault notes

After implementation:
- Update `.knowledge/notes/three-extension-modes.md` status and content
- Update `.knowledge/maps/open-threads-map.md`

## Expected outcome

- `:tune` mode enforced at compile time with type hierarchy awareness
- `:sealed` mode blocks all extensions
- Both tested in `tries/four.cljc` on CLJ
- CLJS build still compiles clean (`npx shadow-cljs compile main`)
- Type registry threading is clean and doesn't pollute the pure function signatures unnecessarily
