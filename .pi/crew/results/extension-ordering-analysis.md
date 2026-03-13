# Extension Ordering Semantics — Analysis & Recommendation

**Scope:** `effective-cases` in `src/thetis/functions/spec.clj`  
**Issue:** When two namespaces extend the same generic with `:override` mode,
precedence is determined by **reverse alphabetical order of namespace name**, not by registration time.

---

## The Code

```clojure
(let [sorted-ext-keys (sort (keys exts))
      ext-cases (mapcat #(get exts %) (reverse sorted-ext-keys))]
  (vec (concat ext-cases decl)))
```

`(sort (keys exts))` → ascending alphabetical  
`(reverse ...)` → descending, so namespaces like `"zz.foo"` beat `"aa.foo"`

---

## Q1 — Is This Actually a Problem in Practice?

**It's narrow but real.** The conflict only surfaces under `:override` mode — the only
mode that allows two namespaces to register an implementation for the *same type* on the
*same generic*. `:extend` and `:refine` modes explicitly block duplicate-type overrides at
compile time (`check-extend-mode`, `check-refine-mode`), so the ordering between namespaces
is moot there.

Scenarios where `:override` + multi-ns conflict matters:
1. **Adapter layers** — a library provides a generic with `:override` mode; an app namespace
   and a third-party integration both extend the same type (e.g., both extend `:string`).
2. **`fork`-then-extend** — `fork` resets mode to `:override`. Two namespaces extending
   a forked generic can conflict if they target overlapping types.
3. **REPL-driven development** — a developer extends from two namespaces interactively,
   expecting the last `eval`'d call to win.

In practice this is uncommon because sensible library design avoids having multiple external
namespaces override the *same* type on *the same* generic. But it's a real footgun when it
does happen.

---

## Q2 — Could Temporal Ordering Be Implemented?

**No, without breaking idempotent rebuilds — and that would be a serious regression.**

The `extension-cases` map is a plain Clojure hash map `{ns-string → [cases]}`.
Hash maps carry **no insertion order** and **no timestamps**. There is nothing in the
data structure to recover temporal ordering.

More fundamentally, the entire storage design is built around **idempotency**:
`remove-ns-contributions` followed by re-extension must reproduce the identical
state (tested explicitly in `idempotent-rebuild-test`). This is essential for the
incremental compilation model where hot-reloading a namespace means:

1. Strip that namespace's contributions
2. Re-execute its code (re-registers the extension)

If temporal ordering were used, the re-registered namespace would be "most recent"
and would win over a namespace that was loaded earlier and hasn't changed. That's
indeterminate and breaks the rebuild contract.

**Verdict:** Temporal ordering is architecturally incompatible with the rebuild model.
Implementing it would require a vector of `[ns timestamp cases]` tuples, forfeiting
idempotency.

---

## Q3 — What Would Break if We Changed the Ordering?

| Change | Impact |
|---|---|
| Switch to temporal ordering | Breaks `idempotent-rebuild-test`; makes behavior dependent on load order, which varies between REPL sessions and build targets |
| Switch to insertion-order (no sort) | Same problem — Clojure hash maps don't expose insertion order; would silently give different results across JVM versions |
| Switch to alphabetical ascending (reverse the `reverse`) | Low-probability breakage of existing code that relies on current behavior; semantically equivalent wrongness |
| Document current behavior clearly | Zero breakage |

---

## Q4 — Document or Change?

**Document it. Don't change the ordering — but fix the misleading comment.**

The alphabetical sort is the *right* choice for this architecture because it is:
- **Deterministic** — same order every JVM start, every hot reload
- **Idempotency-preserving** — remove + re-add doesn't shift precedence
- **Stable across build targets** — doesn't depend on classpath scan order or file timestamps

The real problem is the **misleading comment** on lines 17–19:

```clojure
;; This preserves the precedence semantics: most recently added wins.
;; Extension ordering is by sorted namespace keys (deterministic).
```

"Most recently added" directly contradicts "sorted namespace keys." The comment
incorrectly implies temporal semantics. It should read:

```clojure
;; Extension ordering is alphabetical by namespace name (descending — later names win).
;; This is deliberately deterministic and rebuild-safe, not temporal.
;; Consequence: "zz.my-overrides" beats "aa.lib-defaults" regardless of load order.
```

Similarly, the `effective-cases` docstring says "latest extensions first" — "latest"
should be "alphabetically last namespace".

---

## Recommendation

1. **Keep the current implementation unchanged.**  
   The sort is correct; changing it risks breakage and gains nothing.

2. **Fix the misleading comment** in `spec.clj` lines 17–19 and the `effective-cases`
   docstring to accurately describe alphabetical-descending order rather than temporal order.

3. **Add a note to the API docs / `doc/API.md`** under `:override` mode:
   > When multiple namespaces extend the same type on an `:override` generic, the
   > namespace whose name sorts last alphabetically wins. This ordering is intentional
   > — it ensures consistent behavior across hot reloads and incremental rebuilds.
   > If deterministic precedence is important, structure your namespaces accordingly
   > (e.g., `my.app.overrides` > `my.app.defaults`).

4. **Optional:** Add a lint/warn path that emits a warning when `merge-cases` detects
   that two different namespaces are registering an implementation for the same
   `[type arity]` pair under `:override` mode. This surfaces the ambiguity at
   extension time rather than silently resolving it alphabetically.

---

## Summary

The reverse-alphabetical ordering is a valid, principled design choice that preserves
rebuild idempotency — the most important invariant in the incremental compilation model.
The only real bug is a misleading comment that says "most recently added wins" when the
actual semantics are "alphabetically latest namespace wins." Fix the comment; leave the
code alone.
