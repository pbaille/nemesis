# Thetis Test Audit Report

**Auditor:** test-auditor  
**Date:** 2026-03-13  
**Verdict:** ⚠️ Proof-of-concept exercises masquerading as tests. Critical API surface is untested; CLJS coverage is illusory; no regression safety net exists.

---

## 1. Coverage

### Public API in `thetis.core`

| Macro/Fn | Tested? | Notes |
|---|---|---|
| `defg` | ✅ Yes | Thoroughly exercised in one/four/five |
| `generic+` | ✅ Yes | Tested across all tries |
| `type+` | ✅ Yes | one.cljc |
| `fork` | ✅ Yes | one/two/three.cljc |
| `fork+` | ✅ Yes | one/two.cljc |
| `deft` | ✅ Yes | one/two.cljc |
| `defguard` | ✅ Yes | five.cljc |
| `thing` | ⚠️ Partial | two.cljc — only non-variadic, single `thing` instance |
| `register-type` | ⚠️ Indirect | Only invoked internally by `deft`; standalone not tested |
| `implements?` | ❌ ZERO | Never called anywhere |
| `reset-state!` | ❌ ZERO | Build hook, never invoked in tests |
| `prepare-state!` | ❌ ZERO | Incremental build hook, never invoked in tests |

### `thetis.types` namespace — **COMPLETELY UNTESTED**

The entire `thetis.types` public API has zero test coverage:

- `parents`, `childs`, `childof`, `parentof` — hierarchy queries
- `classes` — type→class resolution
- `cyclic?`, `all-paths` — cycle detection
- `symbolic-pred`, `predmap`, `compile-pred-map`, `builtin-preds` — predicate compilation
- `isa` macro
- `get-reg`, `get-type-state`, `get-type`
- `clj-base-types`, `cljs-base-types`, `groups`, `preds-symbols` (re-exports)

You could delete the entire `thetis.types` namespace and the tries would still pass.

### Other untested internal surfaces

- `compiler.data` — `make-compiler`, `clj-compiler`, `cljs-compiler`
- `compiler.core/implementers-map`
- `compiler.core/get-class-cases`
- The `thetis.state/debug` atom

---

## 2. Test Quality

### The fundamental problem: `assert` is not a test framework

The tries use raw Clojure `assert` with no test framework, no named tests, no runner integration.

Consequences:
1. **Failures lose context.** When an assertion fails, you get an `AssertionError` from a line number. In CLJS this is even worse — assertion messages may be stripped.
2. **`assert` can be silenced.** In CLJS with `:advanced` optimizations, `*assert*` may be false, or the ClojureScript compiler setting `:elide-asserts true` disables all asserts entirely. In that configuration, every "test" trivially passes.
3. **No test runner.** You can't run `lein test`, `clj -T:build test`, or `cljs-test-runner`. The tries run only when their namespaces load — and only if something explicitly requires them.

### Truthiness checks vs equality

Several assertions only check truthiness, not values:

```clojure
;; two.cljc — checks that bub is truthy, not that it equals anything specific
(assert (one/bub 1 1))
(assert (one/->bub 1))
(assert (one/->bub (one/bib 1)))
```

`(bub 1 1)` creates a record and records are truthy. This asserts construction doesn't throw — not that the result is correct.

### What's genuinely asserted vs what just runs

**Good assertions** (actual value checks):
```clojure
(assert (= "I am vec" (g1 [])))
(assert (= [:g2vec [] 1] (g2 [] 1)))
(assert (= [:pos 42] (describe 42)))
```

**Weak assertions** (just checks truthiness or negation):
```clojure
(assert (valid [1 2 3]))          ; Is valid truthy? Not what it returns
(assert (not (valid [nil 1 nil]))) ; Is result nil/false? Value unknown
(assert (one/bub 1 1))            ; Truthy? Doesn't verify (bub 1 1) is the right thing
```

---

## 3. Edge Cases

### What's tested (happy paths only)
- Basic type dispatch: `:vec`, `:coll`, `:number`, `:string`, `:keyword`, `:symbol`, `:nil`
- Group dispatch: `:coll` covers `#{}`, `()`, etc.
- Set-type dispatch: `#{:keyword :symbol}`
- Multi-arity dispatch
- Extension modes: `:extend`, `:refine`, `:sealed`, `:override`
- Guard dispatch: `:positive`, `:non-empty-vec`, `:non-empty-string`, `:non-empty-coll`

### What's not tested (edge cases)

**No-default generics:** Every `defg` in the tries has a default case. What happens when you call a generic with no default on an unsupported type? Presumably a protocol dispatch error, but the error message, behavior, and recovery are untested.

**Nil edge cases:** Only one test (`nil-not-overiden-by-default-case` in one.cljc). No test for: nil inside collections in dispatch, nil as generic argument in multi-arity forms, nil with guards.

**Multi-namespace extension ordering:** `extension-cases` are ordered by sorted namespace key (most recent wins). No test exercises two `generic+` calls from different namespaces competing on the same type. Break the sort order or precedence logic and nothing catches it.

**Cyclic type hierarchies:** `cyclic?` and `all-paths` exist specifically for cycle detection. Never triggered in tests.

**Guard ordering:** When multiple guards could match, "first match wins." No test puts two overlapping guards on the same generic to verify ordering is stable.

**`deft` with multiple `:belongs-to` groups:** `point` uses `[:belongs-to [:map]]` (single group). Multiple groups in `:belongs-to` is untested.

**`thing` with variadic generics:** The `thing` macro compiles to a `reify`. Whether variadic generics work inside `thing` is untested.

**Error recovery:** What is the compiler state after a failed `macroexpand`? The state is mutable. No test verifies state integrity after a failed extension.

**`generic+` with unknown arity:** The spec says this should throw (`assert (every? ... arities)`). Never triggered in tests.

**Overriding default case via `generic+`:** Blocked by `allow-default-overides` check. The compile-time check is verified (one.cljc `#?(:clj ...)`), but only on CLJ.

---

## 4. CLJ vs CLJS — CLJS Coverage Is Illusory

This is the most serious finding.

### What the reader conditionals actually do

```clojure
#?(:clj (tap> "thetis one"))          ; no-op
#?(:clj (state/get :functions))       ; state introspection — CLJ only
#?(:clj (assert ... macroexpand ...)) ; compile-time error checks — CLJ only
```

Every `#?(:clj ...)` is a CLJ-only block. There are **zero `#?(:cljs ...)` blocks** across all five tries files. No CLJS-specific assertions exist.

### The core.cljs problem

```clojure
;; src/thetis/core.cljs — the entire file:
(ns thetis.core)
(defonce prototypes (atom {}))
(defonce guard-impls (atom {}))
```

The ClojureScript implementation relies on macros running in CLJ-land (via shadow-cljs) that emit CLJS code targeting the prototype chain. The entire `thetis.compiler.forms/cljs-extend1` mechanism — which manipulates prototype properties via `js*` — is the CLJS dispatch backbone, and it is **never exercised by any test infrastructure**.

### What this means in practice

The tries are `.cljc` files. When run under shadow-cljs:
1. The macro expansions happen in CLJ (the macros are CLJ-only, core.cljs just has atoms)
2. The emitted code runs in CLJS/JS
3. BUT: there is no shadow-cljs test runner configured, no `:test-runner` build target visible in the tries

The compile-time mode enforcement (`check-extend-mode`, `check-refine-mode`) that uses `macroexpand` is tested **only in CLJ**. In CLJS, these checks run at ClojureScript compile time inside shadow-cljs — but whether they throw correctly in the CLJS compilation path is untested.

**The CLJS prototype dispatch path (`js*` prototype assignment, `cljs-extend1`) is entirely untested.**

---

## 5. Regression Coverage

Would these tests catch a real regression? Depends on what you break:

| What you break | Tests catch it? |
|---|---|
| `defg` dispatch stops working | ✅ Yes |
| `generic+` extension stops working | ✅ Yes |
| Extension mode `:sealed`/`:extend`/`:refine` stops blocking overrides | ✅ Yes (CLJ only) |
| Guard dispatch stops working | ✅ Yes |
| Fork isolation breaks (fork affects parent) | ✅ Yes |
| `implements?` completely removed | ❌ No |
| `thetis.types/parents` returns wrong ancestors | ❌ No |
| `thetis.types/isa` compiles wrong predicates | ❌ No |
| `reset-state!` fails to clear state | ❌ No |
| `prepare-state!` duplicates contributions on reload | ❌ No |
| CLJS prototype dispatch emits wrong JS | ❌ No |
| `register-type` `:groups` parameter ignored | ❌ No |
| Extension case ordering inverted (precedence bug) | ❌ No |
| `thing` reify emits wrong method names | Possibly |
| `deft` field destructuring in impls broken | ✅ Partial |
| `guard-impls` atom accumulates duplicates on reload | ❌ No |

---

## 6. The Most Critical Gaps — What to Break Undetected

### 1. Delete `thetis.types` entirely
The entire hierarchy query API (`parents`, `childs`, `isa`, `symbolic-pred`, etc.) has zero test coverage. Delete the file, fix the require errors in the compiler, and all tries pass.

### 2. Break CLJS dispatch
Change `cljs-extend1` to emit incorrect prototype property names. All five tries run fine (they load their asserts via CLJ-side macroexpansion). No test runs in a real CLJS environment to verify the emitted JS works.

### 3. Corrupt `implements?`
Change `implements?` to always return `false` or `nil`. Zero tests use it. Ship it.

### 4. Break extension-case ordering
In `spec/effective-cases`, cases are ordered: `(reverse sorted-ext-keys)` for extensions, then declaration cases. Reverse the reversal. Multi-extension semantics break, but no test has two `generic+` calls from competing namespaces on the same type — so nothing catches it.

### 5. Break `prepare-state!` / reload semantics
If `prepare-state!` fails to call `reset-prepared!`, or if `remove-ns-contributions` has a bug, repeated hot-reloads during development would accumulate duplicate function registrations. No test exercises a reload cycle.

### 6. Break `register-type :groups` membership
In `register-type-contribution`, if group membership (`groups` parameter) were silently ignored, custom types would never participate in group dispatch (`:coll`, `:map`, etc.). The one `deft` test using `:belongs-to` (`point`) would break, but standalone `register-type` with groups is never tested independently.

---

## Recommendations (prioritized)

1. **Adopt `clojure.test`** with `deftest`/`is`. Raw `assert` gives no structure, no runner, no CLJS protection against `:elide-asserts`.

2. **Test `thetis.types`** — at minimum `parents`, `childof`, `parentof`, `classes`, `isa`, and `symbolic-pred`. This is load-bearing infrastructure.

3. **Test `implements?`** — it's the user-facing predicate for type checking and has zero coverage.

4. **Set up a real CLJS test runner** (shadow-cljs `:karma` or `:browser` test target). Until then, CLJS correctness is assumed, not verified.

5. **Test extension case ordering** — two `generic+` from different namespaces, verify which wins.

6. **Test error cases** — call a no-default generic with an unsupported type; verify the exception message is useful.

7. **Test `register-type` standalone** — not just via `deft`.

8. **Test reload semantics** — require a namespace twice (or simulate via `prepare-state!`) and verify no duplicate dispatch.
