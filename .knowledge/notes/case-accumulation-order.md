---
title: case-accumulation-order
created: 2026-02-22
updated: 2026-02-22
type: open-thread
subsystem: generics
status: resolved
tags: [case-ordering, precedence, spec-registration]
related: ["[[namespace-keyed-spec-registration]]"]
---

# Case Accumulation Order

## Problem (Historical)

The original `notes.org` raised a concern about how cases accumulate:

> I could keep all cases, currently I'm using impl.registry/conj-case that is removing some previous cases but it should not be necessary. Cases should be simply added, each batch of cases should be reversed and prepended to the cases vector OR append batches non reversed and reverse the whole thing for computing extension-class-cases.

The worry: `conj-case` was destructively removing previous cases during accumulation, losing information. The ordering of cases matters for precedence (first match wins, like `cond`).

## Resolution

The namespace-keyed spec registration model (see [[namespace-keyed-spec-registration]]) resolved this completely by restructuring how cases are stored:

```clojure
{:declaration-cases [case1 case2]                          ;; from defg, immutable
 :extension-cases {"ns.b" [case3 case4] "ns.c" [case5]}}  ;; from generic+, keyed by ns
```

### Ordering Rules (poly.functions.spec/effective-cases)

1. Extension cases come first (higher precedence)
2. Extensions are ordered by **reverse-sorted namespace keys** — deterministic across rebuilds
3. Within a namespace, cases appear in call order (`generic+` invocation order)
4. Declaration cases come last (lowest precedence, except default)

### Precedence in class-extensions

`class-extensions` reverses the effective case list then reduces with `assoc` on `[class arity]`. Net effect: the **first** case in the effective list (= latest extension) wins for a given class+arity pair.

### Why This Resolves the Original Concern

- No cases are ever removed during accumulation (`merge-cases` only appends)
- Batches are kept intact per namespace key
- The flat `:cases` vector is recomputed from the structured storage on every mutation
- `prepare-ns!` cleans a namespace's contributions before re-expansion → idempotent

## Remaining Nuance

The reverse-sorted namespace ordering is deterministic but somewhat arbitrary. If namespace `aaa.ext` and `zzz.ext` both extend the same type on the same generic, `zzz.ext` wins because it sorts later → appears earlier in the reversed list. In practice this matches Clojure require ordering (alphabetical) in most projects, but it's not guaranteed to match actual load order.

This is unlikely to matter — if two namespaces extend the same type on the same generic, you have a real design problem regardless of ordering.

## Origin

From `notes.org`:
> cases should be simply added, each batch of cases should be reversed and prepended to the cases vector OR append batches non reversed and reverse the whole thing for computing extension-class-cases
