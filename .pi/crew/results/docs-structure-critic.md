# docs-structure-critic report

**Scope:** README.md, doc/API.md, all `src/thetis/` source files (excluding tries/)  
**Method:** Full read + cross-reference of docs against implementation

---

## TL;DR

The docs are generally good but have a few real accuracy bugs (missing types, one inconsistent call-syntax example), a completely undocumented user-facing namespace (`thetis.types`), and several silent behavioral surprises. The structure has a thin-file problem, a dead-code duplication, and one dependency-direction smell. No circulars. Public/private boundary is not enforced (`thetis.utils.*`, `thetis.compiler.*` are all public-by-default).

---

## 1. Documentation — Accuracy of doc/API.md

### 1a. Missing type keywords from the API table

The API.md type keyword table is **incomplete**. Three types in `thetis.types.data` are silently absent:

| Missing keyword | CLJ class | CLJS class |
|---|---|---|
| `:boolean` | `clojure.lang.Boolean` | `boolean` |
| `:map-entry` | `clojure.lang.MapEntry` | `MapEntry` |
| `:default` | `java.lang.Object nil` | `default` |

`:default` is particularly important — it's the catch-all fallback used by the protocol dispatch layer. Users who write `defg` bodies may encounter it in protocol names but have no docs explaining what it is.

### 1b. `register-type` call syntax — potentially misleading

**API.md shows:**
```clojure
(register-type :my-type
  {:classes [my.package.MyClass]
   :groups [:coll]
   :impls [(encode [x] (.serialize x))]})
```

**Actual macro signature:**
```clojure
(u/defmac register-type [tag & {:keys [classes groups impls]}] ...)
```

The docs use the **map-as-kwargs** style valid in Clojure 1.11+ (CLJ-2603). The `deft` macro also uses this style internally. However, it's inconsistent with every other example in the README (which uses flat keyword args like `:mode :sealed`). A user unfamiliar with CLJ 1.11 map-as-kwargs will be confused about why a single map is passed. At minimum, a note is warranted.

### 1c. `fork` 1-arg form is undocumented

The `fork` macro has two arities:
```clojure
([name]
 `(fork nil ~name))       ; ← creates fork with same local name, strips ns
([new-name original-name]
 ...)
```

The single-arg form is never mentioned in the docs. Its semantics (strips the namespace, creates a local fork) may be intentional but is invisible.

### 1d. `deft` cast generic has no default case — not warned

`deft` generates:
```clojure
(defg ->rgb [x]
  :my.ns/rgb x       ; identity
  :map (map->Rgb x)) ; cast from map
```

There is **no default case**. If a user calls `(->rgb "a string")`, protocol dispatch finds no implementation and throws a `ClassCastException` or `IllegalArgumentException` depending on platform — not an `ex-info`. The docs show extending the cast function for `:vec` but never warn that all other unhandled types will throw a protocol-dispatch failure rather than a meaningful error. This is a usability trap.

### 1e. Extension case ordering is not documented

`functions.spec/effective-cases` applies extensions in **reverse alphabetical order by namespace string**, not by definition time:

```clojure
(let [sorted-ext-keys (sort (keys exts))
      ext-cases (mapcat #(get exts %) (reverse sorted-ext-keys))]
  ...)
```

If two different namespaces extend the same generic, precedence is alphabetical (later in alphabet = higher priority). This is surprising and completely undocumented. Users who extend generics from multiple namespaces will hit this with no explanation.

### 1f. Guard dispatch breaks the "zero overhead" claim

README says: *"Zero overhead on CLJ — Compiles to protocol dispatch (JVM interface calls). No runtime reflection."*

This is only true if no `defguard` types are used. Once any guard case exists for a generic, the generated `defn` uses two-phase dispatch:

```clojure
(let [guards# (get-in (deref guard-sym) ['~fullname ~arity-key])]
  (if guards#
    (or (some (fn [g#] (when ((:pred g#) ...) ...)) guards#)
        protocol-call)
    protocol-call))
```

This is an atom deref + `some` over a vector on every call. The README should qualify: "zero overhead unless predicate guards are active."

---

## 2. Documentation — Undocumented public API

### 2a. `thetis.types` namespace — entirely absent from docs

`thetis.types` has a docstring: *"User-facing type operations for thetis."* It exports:

- `get-reg`, `get-type-state`, `get-type`  
- `all-paths`, `cyclic?`  
- `childs`, `parents`, `childof`, `parentof`, `classes`, `all-types`  
- `symbolic-pred`, `symbolic-pred-body`, `predmap`, `compile-pred-map`, `builtin-preds`  
- `isa` macro — runtime type predicate generator  

None of these appear in README or API.md. The `isa` macro is particularly useful (generates `(fn [x] (when (instance? T x) x))` forms) but is invisible to users. If this namespace is not intended for users, remove the "User-facing" claim from its docstring and make it `thetis.types.api` or similar.

### 2b. Docstring coverage in public vars

Missing docstrings on public vars:

| Namespace | Var | Has docstring? |
|---|---|---|
| `thetis.state` | `state0`, `state`, `lambda-case-compiler*`, `debug` | ✗ |
| `thetis.state` | `env`, `form`, `cljs?`, `clj-state`, `cljs-state`, `current`, `get`, `get-in`, `compilation-target` | ✗ |
| `thetis.types` | `clj-base-types`, `cljs-base-types`, `groups`, `preds-symbols`, `get-reg`, `get-type-state`, `get-type`, `symbolic-preds->or-form`, `symbolic-pred-body`, `compile-pred-map`, `predmap`, `builtin-preds` | ✗ |
| `thetis.compiler.data` | `make-compiler` | ✓ | `clj-compiler`, `cljs-compiler` | ✓ |
| `thetis.functions.registry` | `get!`, `add-spec`, `extend-spec`, `clone-spec`, `remove-ns-contributions`, `implementers-map`, `get-class-cases` | mixed |
| `thetis.utils.misc` | Most functions | partial |

The core public macros in `thetis.core` all have docstrings ✓.

---

## 3. Structure

### 3a. `thetis.utils.misc` is a grab-bag

At ~200 LOC, `utils.misc` contains:
- CLJS JS interop (`cljs_prototype-assoc-form`)
- Symbol manipulation (`sym`, `with-ns`, `fullname`, `ns-sym`)
- Function case normalization (`fn-cases_normalize`, `fn-case_bodify`)
- Argv manipulation (`argv_variadic?`, `argv_variadify`, etc.)
- Binding pattern utilities (`binding-pattern_ensure-top-level-sym`)
- Macro helpers (`defmac`, `parse-fn`)
- Debug utils (`pp`, `pretty-str`)

These concerns are unrelated. A new contributor has no idea where to look for what. Suggested split: `utils.symbols`, `utils.fn`, `utils.macros`.

### 3b. Dead code duplication: `cljs_prototype-assoc-form`

`utils.misc` defines:
```clojure
(defn cljs_prototype-assoc-form [obj meth impl]
  (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))
```

`compiler.forms` also defines it locally inside a `letfn`:
```clojure
(letfn [(cljs_prototype-assoc-form [obj meth impl]
          (list 'js* "~{}[\"prototype\"][~{}] = ~{}" obj meth impl))
        ...]
  (defn cljs-extend1 ...))
```

But `cljs-extend1`'s body uses `u/cljs_prototype-assoc-form` (the qualified import), not the local `letfn` binding. The `letfn` version is **never called**. It's dead code, identical to the `utils.misc` version, and actively confusing.

### 3c. `compiler.forms` depends on `thetis.types` (the impure facade), not `thetis.types.core`

`thetis.compiler.forms` requires `[thetis.types :as types]` and calls `types/symbolic-pred-body` and `types/get-reg`. `thetis.types` reads from `thetis.state` (mutable global state). The compiler layer should be pure — it receives state as a value (the `compiler` map). By reaching through to `thetis.types/get-reg`, `compiler.forms` has an implicit state dependency that bypasses the clean value-passing design used everywhere else. This is the one place where the "pure compiler value" abstraction leaks.

Specifically, `guard_registering` in `compiler.forms` calls:
```clojure
(types/symbolic-pred-body (types/get-reg) base gsym)
```

This could instead use the type registry from the compiler value that's already passed around.

### 3d. `thetis.compiler.data` is nearly empty

Three exported items: `make-compiler`, `clj-compiler`, `cljs-compiler`. These are used in tests but not in main runtime. This file is mostly a test utility living in main sources. Either delete and inline into tests, or document it as the "entry point for pure compiler construction."

### 3e. Dependency flow summary

```
thetis.core
  → thetis.state          (impure: global atom, expansion context)
  → thetis.compiler.core  (pure: compiler operations)
  → thetis.compiler.forms (code generation: depends on types facade!)
  → thetis.functions.parse

thetis.compiler.forms
  → thetis.types          ← PROBLEM: impure state-reading facade
  → thetis.types.core     (pure: type registry operations)

thetis.types              (user-facing facade, reads thetis.state)
  → thetis.state
  → thetis.types.core
  → thetis.types.data

thetis.state
  → thetis.types.core
  → thetis.types.data
  → thetis.utils.expansion
```

No cycles. But the `compiler.forms → thetis.types` dependency is a layering violation.

---

## 4. Public vs Private API Surface

**Observation:** Clojure has no access control. Everything in `thetis.compiler.*`, `thetis.functions.*`, `thetis.utils.*`, `thetis.types.*` is effectively public. There is no `^:no-doc` metadata, no `internal` namespace convention, and no indication in the README of which namespaces users should require.

**Expected user surface:**
- `thetis.core` — all macros (documented ✓)
- `thetis.types` — type hierarchy queries (undocumented ✗)

**Unexpected exposure:**
- `thetis.state/lambda-case-compiler*` — an undocumented extension point atom
- `thetis.state/debug` — bare `(defonce debug (atom nil))` with no explanation
- `thetis.compiler.data/clj-compiler` and `cljs-compiler` — visible compiler seeds
- `thetis.compiler.core/implementers-map` — interesting introspection fn, zero docs

---

## 5. Naming Consistency Issues

| Issue | Location |
|---|---|
| `childs` in `thetis.types` vs `children` in `thetis.types.core` | `thetis.types/childs` wraps `types/children` but renames it |
| `defg` / `generic+` vs standard Clojure `defprotocol` / `extend-type` naming | intentional DSL, but jarring |
| `deft_impl-bind-fields` uses underscore separator; other helpers use camelCase mix like `fn-cases_normalize` | inconsistent hybrid |
| Protocol names `Ifoo_2`, method names `p_foo_2` leaked to namespace — convention undocumented | — |
| `cljs_prototype-assoc-form` (underscore) vs `cljs-extend1` (hyphen) | inconsistent in same file |

---

## 6. Surprising Behaviors for New Contributors

1. **`defmac` generates 3 vars for every macro definition**: a `-fn` function, a `-fn*` partial, and the macro itself. E.g., `defg` creates `defg-fn`, `defg-fn*`, `defg`. Undocumented in `utils.misc`. Contributor sees these in the REPL and has no idea why.

2. **`thetis.state/expanding` is a macro that binds `&env` and `&form`**: All macros must go through `defmac` which wraps them. A contributor writing a new macro without `defmac` will get nil for `(state/env)` and `(state/form)`, causing subtle CLJS detection failures.

3. **Incremental build safety via `prepare-ns!`**: Every macro entry point calls `prepare-ns!` which removes all previous contributions from that namespace on first call per build pass. This is critical for correctness but has no architecture docs. A contributor adding a new entry-point macro that forgets `prepare-ns!` will get stale state on incremental rebuilds with no error.

4. **`effective-cases` requires `init-spec` to have been called**: There's an `assert` for this, but the initialization split (`declaration-cases` + `extension-cases`) is not explained. A contributor touching spec manipulation is likely to construct a raw spec and hit the assert.

5. **`(do :label ...)` as a code organization idiom**: Used throughout `compiler.forms` and `utils.misc`. This is a personal style convention (no-op `do` with keyword label as ersatz section header) that confuses anyone unfamiliar with it. Not documented anywhere.

---

## Summary Table

| Category | Finding | Severity |
|---|---|---|
| Docs | `:boolean`, `:map-entry`, `:default` missing from type table | Medium |
| Docs | `register-type` map-vs-kwargs inconsistency | Low |
| Docs | Extension ordering (alphabetical by ns) undocumented | High |
| Docs | Guard dispatch breaks "zero overhead" claim | Medium |
| Docs | `deft` cast fn has no default — silent crash | Medium |
| Docs | `thetis.types` namespace entirely absent | High |
| Docs | `fork` 1-arg form undocumented | Low |
| Structure | `compiler.forms` → `thetis.types` (impure facade) | Medium |
| Structure | `utils.misc` grab-bag | Low |
| Structure | `cljs_prototype-assoc-form` dead code duplication | Low |
| Structure | `compiler.data` barely-populated file | Low |
| API surface | No convention for internal vs public namespaces | Medium |
| Naming | `childs` vs `children` | Low |
| Naming | `(do :label ...)` idiom unexplained | Low |
| Anti-pattern | `defmac` triple-var generation invisible to contributors | Medium |
| Anti-pattern | `prepare-ns!` protocol undocumented | High |
