---
type: task
tags: [open-threads, knowledge-base, design, vault, cleanup]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T06-47-21-683Z_e9cb041d-056c-43a2-bdc0-6117cbcd97ca.jsonl
---

# Catalog open threads from notes.org into vault

## Context

The poly migration is complete:
- Nemesis namespaces deleted, `poly.*` is the sole implementation
- All CLJ tests pass (19 tests, 92 assertions, 0 failures)
- All CLJS tests pass (shadow-cljs, 4 tries files, browser-verified)
- Incremental builds work with `prepare-state!` hook

The project has two `notes.org` files with unresolved design threads that predate the vault system. These need to be extracted into individual atomic notes in `.knowledge/notes/` so they can be tracked, linked, and resolved over time. Some threads may already be partially resolved by the poly migration — those need status updates rather than blind transcription.

## Source files

### `./notes.org` (root)
Contains these open threads:
1. **Forking semantics** — How to represent a forked spec in the compile-time registry. Full clone vs alias. Runtime state cloning.
2. **Case accumulation order** — Should cases be prepended or appended? Current `conj-case` removes some previous cases. Alternative: append batches non-reversed, reverse whole thing for computing extension-class-cases.
3. **Three extension modes (extend/tune/patch)** — Safety semantics for generic extension. `extend` = add only, `tune` = specialize subtypes, `patch` = full override. Some generics could disallow extension entirely.
4. **Runtime state facilities** — Wrap/update behavior at generic or implementation level. Extract methods to build objects on the fly.
5. **defg-as-defn** — A generic with only default implementations could just be a normal function that becomes generic when extended. Enforces object-as-first-arg convention.

### `./src/poly/notes.org`
Minimal — just an outline of the poly compiler model (types, functions, host). May not need individual notes since the architecture is now implemented.

## Existing vault notes

These notes already exist in `.knowledge/notes/` and may overlap with or resolve some threads:
- `cljs-incremental-build-problem.md` — status: **resolved**
- `expansion-state-keep-as-is.md` — relates to the "expansion-state dynamic variable could go away" thread in notes.org
- `namespace-keyed-spec-registration.md` — resolves part of the case accumulation problem
- `poly-compiler-as-value.md` — relates to the runtime state facilities thread
- `poly-migration-strangler-fig.md` — migration strategy (complete)
- `poly-replaces-nemesis.md` — migration status
- `state-bookkeeping-needs.md` — relates to state mutations
- `state-shape-aligned-with-poly-compiler.md` — architecture alignment

## Existing map

- `.knowledge/maps/architecture-map.md` — may need new entries for open threads

## Task

### 1. Triage each open thread

For each thread in `notes.org`, determine:
- Is it already captured in an existing vault note? → update that note's status/content
- Is it resolved by the poly migration? → create note with `status: resolved` and rationale
- Is it still open? → create note with `status: active` and `type: open-thread`

### 2. Create atomic notes

For each unresolved thread, create a note in `.knowledge/notes/` with proper frontmatter:

```yaml
---
title: descriptive-title
created: 2026-02-22
updated: 2026-02-22
type: open-thread
subsystem: generics | extension | types | poly | meta
status: active | resolved | parked
tags: [hyphenated-tags]
related: ["[[related-note]]"]
---
```

Each note should include:
- The problem statement (from notes.org)
- Current state (what poly has or hasn't changed about this)
- Concrete code examples where possible (ground in actual macro expansions)
- Links to related existing notes

### 3. Create or update `open-threads-map.md`

Create `.knowledge/maps/open-threads-map.md` as a living index of all unresolved design problems, linking to individual notes and organized by subsystem.

### 4. Review `tries/three.cljc`

During CLJS verification, we noticed `tries/three.cljc` uses `:str` instead of `:string` as a type key. This is a pre-existing bug — the extension silently matches nothing. Fix it to `:string` and verify tests still pass on both CLJ and CLJS.

### 5. Consider archiving notes.org

Once all threads are captured in the vault, the `notes.org` files become redundant. Either delete them or add a note at the top pointing to the vault as the canonical location.

## Expected outcome

- 3-5 new notes in `.knowledge/notes/` for genuinely open threads
- 1-2 existing notes updated with resolution status
- `open-threads-map.md` created in `.knowledge/maps/`
- `:str` → `:string` fix in `tries/three.cljc`
- Clear picture of what design problems remain unresolved
