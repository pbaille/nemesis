---
type: task
tags: [cljs, shadow-cljs, poly-core, cross-platform, verification]
status: done
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T06-29-36-653Z_0288fef7-9e45-4467-89b9-12869d73dc56.jsonl
---

# Verify poly works under ClojureScript via shadow-cljs

## Context

Nemesis has been deleted. Poly is now the sole implementation. All CLJ tests pass:

```
poly.tries.{one,two,three,four}  — all assert ok
unit tests: 19 tests, 92 assertions, 0 failures
```

But ClojureScript compilation has **not been tested yet**. This is the first time poly.core macros will expand in a CLJS context. The old nemesis CLJS path worked (nemesis had `core.cljs`, shadow-cljs config, and `tries/two.cljc` as the entry point), but poly's macro layer is new code that may have CLJS-specific issues.

### What already exists for CLJS

- **`src/poly/core.cljs`** — Stub: just `(defonce prototypes (atom {}))`. This is the runtime counterpart; macros come from `poly/core.clj` via `:require-macros`.
- **`shadow-cljs.edn`** — Already updated to reference poly:
  ```clojure
  {:cache-blockers #{poly.core}
   :builds {:main {:build-hooks [(poly.core/prepare-state!)]
                   :modules {:main {:entries [poly.tries.two]}}}}}
  ```
- **`deps.edn`** — Has `:cljs` alias with `shadow-cljs 2.20.16` and `clojurescript 1.11.60`
- **`public/index.html`** — Loads `/js/main.js`
- **All `.cljc` test files** — `poly/tries/{one,two,three,four}.cljc` use reader conditionals for CLJ-only blocks

### CLJS-specific code generation

`poly.compiler.forms` has CLJS-aware code paths:

1. **`cljs-extend1`** — Instead of `(extend Class Protocol {method impl})`, CLJS emits `js*` prototype assignments for non-base types and `unchecked-set` for base types (nil, object, string, number, array, function, boolean, default).
2. **`extension-form`** — Checks `(expansion/cljs? expansion)` to pick CLJ vs CLJS extend form.
3. **Platform detection** — `poly.state/cljs?` checks `(:ns (env))` — the `&env` in CLJS always has `:ns`.

### Known CLJS concerns

- The `defmac` pattern in `poly.utils.misc` emits `(when-not (:ns &env) ...)` — returns nil in CLJS, so macro definitions are CLJ-only (correct behavior for cross-platform macros)
- `poly.state/expanding` binds `*expansion-state*` from `&env`/`&form` — this works in CLJS because shadow-cljs compiles macros in CLJ
- `qualify-symbol` uses `cljs.analyzer/resolve-var` in CLJS context — needs `cljs.analyzer` to be available
- `deft` emits different qualified class symbols for CLJ vs CLJS (e.g., `poly.tries.one/Point` vs `poly_tries_one.Point`)
- The `error-form` helper was in `nemesis.impl.utils` — uses `js/Error` for CLJS, `Exception` for CLJ. It's **not used** by poly.core currently, but if any error path emits error forms, they'll need the platform check.

## Files

- `src/poly/core.clj` — macro layer (the new code to test)
- `src/poly/core.cljs` — runtime stub
- `src/poly/compiler/forms.clj` — code generation with CLJS paths
- `src/poly/state.clj` — expansion state, platform detection, type init
- `src/poly/utils/misc.clj` — `defmac` helper
- `src/poly/utils/expansion.clj` — `cljs?`, `qualify-symbol`
- `src/poly/tries/{one,two,three,four}.cljc` — integration tests
- `shadow-cljs.edn` — build config
- `deps.edn` — dependency config

## Task

### 1. Run shadow-cljs compile

```bash
cd /Users/pierrebaille/Code/WIP/nemesis
npx shadow-cljs compile main
```

This will attempt to compile `poly.tries.two` (which requires `poly.tries.one`) using `poly.core` macros in CLJS mode. Fix any compilation errors.

### 2. Common failure modes to watch for

- **Missing `:require-macros`** — `poly.core.cljs` may need `:require-macros [poly.core]` if the auto-detection doesn't work. The old `nemesis/core.cljs` didn't have it — it relied on consumers using `:include-macros true`. Check whether the current tries files have this.
- **`qualify-symbol` returning nil** — In CLJS, `cljs.analyzer/resolve-var` may behave differently for locally defined symbols vs cross-namespace references. If it returns nil, the spec gets a nil fullname → breaks downstream.
- **`deft` class naming** — The CLJS class name path (`(symbol ns-str (name class-sym))`) was ported from nemesis. Verify it produces correct symbols.
- **`extend-class` in CLJS** — When `register-type` calls `forms/extend-class`, it needs the compiler to have `{:expansion {:env {:ns ...}}}` to trigger CLJS code paths. This should be the case since we're inside `expanding`.
- **Prototype atom reference** — The generated JS should reference `poly.core.prototypes`, not `nemesis.core.prototypes`. The `*prototypes-sym*` defaults to `'poly.core/prototypes`, so this should be correct.

### 3. Run in browser

If compilation succeeds, start a dev server and open in browser:

```bash
npx shadow-cljs watch main
```

Open http://localhost:8080 and check the browser console for assertion failures.

### 4. If everything passes

Try extending the shadow-cljs config to also compile `poly.tries.four` (extension modes test) to verify the full test suite works in CLJS:

```clojure
:modules {:main {:entries [poly.tries.two poly.tries.three poly.tries.four]}}
```

### 5. Incremental build verification

With `prepare-state!` as the build hook (not `reset-state!`), edit a file in `poly/tries/` and verify:
- The recompilation is incremental (not full rebuild)
- The compile-time state remains consistent
- Runtime behavior is correct after the incremental rebuild

This validates that the namespace-keyed spec registration + `prepare-ns!` pattern works correctly in CLJS context.

## Invariants

- `poly.core.clj` macros expand correctly in CLJS context (platform-aware code paths)
- Generated JS correctly references `poly.core/prototypes`
- `poly.state` correctly detects CLJS via `(:ns &env)`
- All assertions in tries files pass at runtime in the browser
- No nemesis references remain in generated code
