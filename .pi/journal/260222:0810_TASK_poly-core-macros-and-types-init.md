---
type: task
tags: [poly-migration, macros, types, poly-core, poly-state]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T19-52-19-501Z_27173321-567b-47e8-9271-86002edc5885.jsonl
---

# Build poly.core macros and types initialization

## Context

Poly is replacing nemesis entirely — same macro API, new namespace. The migration plan (see `260222:0724_DESIGN_poly-succession-migration-path.md`) has 5 steps. Step 1 (complete `poly.state`) is already done — it has expansion state, ns tracking, qualify-symbol, all the nemesis.state functionality.

This task covers steps 2-4: build the user-facing `poly.core` macro layer, absorb `nemesis.impl.forms` into `poly.compiler.forms`, and set up type initialization in `poly.state`.

## What exists already

### `poly.state` (complete)
Already has everything needed:
- `*expansion-state*` dynamic var + `env`, `form`, `cljs?`
- `expanding` / `targeting-cljs` macros
- Atom with `state0` including `:type-state {:base {} :contributions {}}`
- `swap!`, `get`, `get-in`, `reset!`, `compilation-target`
- Namespace preparation: `ns-prepared?`, `mark-ns-prepared!`, `reset-prepared!`
- `qualify-symbol` delegating to `poly.utils.expansion`

### `poly.core` (stub)
Currently just `(defonce prototypes (atom {}))`. Needs macros.

### `poly.compiler.forms` (pure, complete)
Already takes `compiler` as first arg everywhere. Key functions:
- `(declaration compiler spec)` → emits protocol + defn + extend
- `(extension compiler spec)` → emits prototype registration + protocol extension
- `(extend-class compiler class)` → emits extensions for all generics implementing `class`
- `(thing compiler impls)` → emits reify
- `(implements-all? expr specs)` → emits satisfies? checks
- `(implement tag impl)` → emits generic+ call (NOTE: currently emits `nemesis.core/generic+` — needs fixing)
- `(deft_impl-bind-fields fields impl)` → destructuring helper

Uses `*prototypes-sym*` dynamic var (default: `'poly.core/prototypes`) for code generation.

### `poly.compiler.core` (pure, complete)
All state transitions as pure functions:
- `add-function`, `extend-function`, `clone-function`
- `register-type-contribution`, `remove-ns-contributions`
- Query: `get-function`, `get-function!`, `class-extensions`, `type->classes`

### `poly.utils.expansion` (pure)
- `(cljs? state)` — takes explicit expansion state
- `(qualify-symbol state x)` — takes explicit expansion state

### `nemesis.impl.forms` (the bridge to absorb)
Wraps every `poly.compiler.forms` call with:
1. `with-nemesis-prototypes` binding `*prototypes-sym*` to `'nemesis.core/prototypes`
2. `current-compiler` building a compiler value from nemesis state

In poly.core, neither wrapper is needed:
- `*prototypes-sym*` already defaults to `'poly.core/prototypes`
- The compiler value is just `(assoc (state/get) :expansion {:env (state/env) :form (state/form)})`

### `nemesis.types` (to absorb into poly.state)
Key functionality:
- `init!` — populates atom with platform-specific base types from `poly.types.data`
- `classes` — just delegates to `poly.types.core/classes` (nothing special)
- `get-reg` / `get-type` — thin state readers
- Hierarchy queries (childs, parents, etc.) — all delegate to `poly.types.core`
- Predicate compilation (`symbolic-pred`, `isa` macro) — nemesis-specific, needs a poly home

## Files to create/modify

### `src/poly/state.clj` — add type initialization
Add `init-types!` function and call it at load time. Needs to require `poly.types.core` and `poly.types.data`:
```clojure
(defn init-types! []
  (let [clj-types (merge type-data/clj-base-types type-data/groups)
        cljs-types (merge type-data/cljs-base-types type-data/groups)]
    (c/swap! state
             #(-> %
                  (assoc-in [:clj :types] clj-types)
                  (assoc-in [:clj :type-state] (types/make-type-state clj-types))
                  (assoc-in [:cljs :types] cljs-types)
                  (assoc-in [:cljs :type-state] (types/make-type-state cljs-types))))))
(init-types!)
```

### `src/poly/compiler/forms.clj` — fix hardcoded nemesis reference
The `implement` function emits `nemesis.core/generic+`. This needs to emit `poly.core/generic+` instead (or be made configurable like `*prototypes-sym*`).

Search for `nemesis` references:
```bash
grep -n "nemesis" src/poly/compiler/forms.clj
```

### `src/poly/core.clj` — build the macro layer
Port all macros from `nemesis.core`. The pattern for each:

**defmac helper**: `nemesis.impl.utils/defmac` is a macro that:
1. Creates a function version (`name-fn`)  
2. Creates a macro version that binds `*expansion-state*` via `state/expanding`
3. Replaces `&env`/`&form` references with `(state/env)`/`(state/form)` in the body

Either port `defmac` to `poly.utils.misc` or use a simpler version.

**Macro-by-macro migration:**

| nemesis.core macro | What it does | poly.core translation |
|---|---|---|
| `defg` | `prepare-ns!` → `parse` → `register-spec!` → `declaration` | `prepare-ns!` → `parse` → `(state/swap! compiler/add-function spec)` → read-back → `(poly.forms/declaration (current-compiler) spec)` |
| `generic+` | `prepare-ns!` → `parse` → `extend-spec!` → `extension` | `prepare-ns!` → `parse` → resolve fullname → `(state/swap! compiler/extend-function ...)` → read-back → `(poly.forms/extension (current-compiler) spec)` |
| `fork` | `prepare-ns!` → `clone-spec!` → `declaration` | `prepare-ns!` → resolve original → `(state/swap! compiler/clone-function ...)` → read-back → `(poly.forms/declaration (current-compiler) spec)` |
| `fork+` | Emits `(fork ...)` then `(generic+ ...)` | Same pattern, just emit poly.core forms |
| `type+` | Maps implement over impls | `(mapv #(poly.forms/implement tag %) impls)` |
| `thing` | Parse impls → reify | `(poly.forms/thing (current-compiler) impls)` |
| `implements?` | Check protocol satisfaction | `(poly.forms/implements-all? v specs)` |
| `register-type` | Validate → `(state/swap! compiler/register-type-contribution ...)` → emit extend-class + type+ | Same but call poly.core/type+ |
| `deft` | defrecord + constructor + cast fn + register-type | Port directly, emit poly.core forms |

**Helper functions in poly.core (not macros):**

```clojure
(defn current-compiler []
  (assoc (state/get)
         :expansion {:env (state/env) :form (state/form)}))

(defn prepare-ns! [ns-str]
  (when-not (state/ns-prepared? ns-str)
    (state/swap! compiler/remove-ns-contributions ns-str)
    (state/mark-ns-prepared! ns-str)))

(defn get-spec [name]
  (state/get-in [:functions (state/qualify-symbol name)]))

(defn get-spec! [name]
  (or (get-spec name)
      (throw (ex-info (str "Cannot find spec " (pr-str (state/qualify-symbol name)))
                      {:available (keys (state/get :functions))}))))
```

### `src/poly/types.clj` — type queries and predicates
Create a user-facing namespace for type operations. This is where `isa`, `classes`, hierarchy queries, and predicate compilation live. These read from `poly.state` — they're part of the impure shell.

### `src/poly/tries/` — integration tests
Copy `nemesis/tries/{one,two,three,four}.cljc` → `poly/tries/`, replace:
- `nemesis.core` → `poly.core`
- `nemesis.state` → `poly.state`  
- `nemesis.impl.registry` → direct poly.state reads
- Keep the same test assertions

### `src/poly/core.cljs` — ClojureScript stub
```clojure
(ns poly.core)
(defonce prototypes (atom {}))
```

## Key concerns

### The `defmac` macro
`nemesis.impl.utils/defmac` is complex — it creates both a function and macro version, handles expansion state binding, and does `doall-rec` to force lazy sequences while dynamic vars are bound. Options:
1. Port `defmac` to `poly.utils.misc` — cleanest, preserves the dual fn/macro pattern
2. Write simpler macros without the function companion — less code, lose REPL convenience
3. Keep `defmac` in a `poly.utils.macros` namespace

Recommendation: option 1, port to `poly.utils.misc`. The `doall-rec` protection against lazy evaluation + dynamic vars is important.

### The `implement` → `generic+` circular reference
`poly.compiler.forms/implement` emits a `generic+` call. In nemesis, this emits `nemesis.core/generic+`. For poly, it needs to emit `poly.core/generic+`. But `poly.compiler.forms` is pure and shouldn't know about `poly.core`.

Solution: make it configurable like `*prototypes-sym*`:
```clojure
(def ^:dynamic *generic+-sym* 'poly.core/generic+)
```
Or: pass it through the compiler value.

### Type predicate compilation
`nemesis.types` has `symbolic-pred`, `compile-pred-map`, `isa` macro — these compile type checks into optimized `instance?` / `or` forms. They need a poly home. They read the type registry from state, so they belong in the impure shell (poly.types or poly.core).

### `types-syncronisation` in nemesis.impl.forms
When the type registry changes (via `register-type`), `types-syncronisation` re-emits protocol extensions for affected generics. This is currently in `nemesis.impl.forms` but NOT called from anywhere in `nemesis.core` (it was likely removed or never wired up). Verify whether it's dead code before porting.

## Validation

### Unit tests
```bash
clj -Sdeps '{:paths ["src" "test"]}' -M -e '(require (quote [clojure.test :refer [run-tests]])) (require (quote poly.functions.spec-test)) (require (quote poly.incremental-build-test)) (require (quote poly.types.core-test)) (run-tests (quote poly.functions.spec-test) (quote poly.incremental-build-test) (quote poly.types.core-test))'
```

### Nemesis integration tests (must still pass — no regression)
```bash
clj -M -e '(do (require (quote nemesis.tries.one)) (require (quote nemesis.tries.two)) (require (quote nemesis.tries.three)) (require (quote nemesis.tries.four)) (println "NEMESIS TESTS OK"))'
```

### Poly integration tests (new — must pass)
```bash
clj -M -e '(do (require (quote poly.tries.one)) (require (quote poly.tries.two)) (require (quote poly.tries.three)) (require (quote poly.tries.four)) (println "POLY TESTS OK"))'
```

## Invariants

- Pure poly modules (`poly.compiler.*`, `poly.functions.*`, `poly.types.core`, `poly.types.data`, `poly.utils.*`) never read global state
- `poly.state` is the single impure namespace (atom + dynamic vars)
- `poly.core` macros are the bridge: read/write poly.state, call pure compiler functions
- Nemesis still works (no regression) — it's not deleted yet
- Poly and nemesis use separate atoms (`poly.state/state` vs `nemesis.state/state`) — they're independent
- No user-facing API changes: same macro names, same semantics, just `poly.core` instead of `nemesis.core`
