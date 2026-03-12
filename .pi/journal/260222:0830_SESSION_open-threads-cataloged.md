---
type: session
tags: [vault, open-threads, notes-org, cleanup, three-cljc]
---

# Open Threads Cataloged from notes.org

## What Happened

Executed the task from `260222:0754_TASK_catalog-open-threads-from-notes-org.md`.

### Triage Results

| Thread from notes.org | Status | Action |
|---|---|---|
| Forking semantics | **Active** | Created `forking-semantics.md` — full clone works, runtime cloning + mode inheritance open |
| Case accumulation order | **Resolved** | Created `case-accumulation-order.md` — resolved by namespace-keyed spec registration |
| Three extension modes | **Active** | Created `three-extension-modes.md` — `:extend`/`:patch` done, `:tune` + `:sealed` open |
| Runtime state facilities | **Parked** | Created `runtime-state-facilities.md` — interesting but CLJ/CLJS divergence makes it hard |
| Expansion state → functional | **Resolved** | Already in `expansion-state-keep-as-is.md` — no new note needed |
| defg-as-defn | **Parked** | Created `defg-as-defn.md` — architecturally complex, not blocking |

### Key Discovery

Extension modes are more implemented than expected! `tries/four.cljc` tests `:extend` mode with compile-time enforcement via `check-extend-mode` in `poly.functions.spec`. `:patch` is the default. `:tune` is mentioned in error messages but has no implementation.

### Files Created/Modified

**New notes (5):**
- `.knowledge/notes/forking-semantics.md`
- `.knowledge/notes/case-accumulation-order.md`
- `.knowledge/notes/three-extension-modes.md`
- `.knowledge/notes/runtime-state-facilities.md`
- `.knowledge/notes/defg-as-defn.md`

**New map (1):**
- `.knowledge/maps/open-threads-map.md`

**Updated:**
- `.knowledge/notes/state-bookkeeping-needs.md` — added related links
- `.knowledge/maps/architecture-map.md` — added open threads reference
- `.knowledge/self/goals.md` — marked task complete, updated focus
- `notes.org` — archived header added
- `src/poly/notes.org` — archived header added

### Bug Fix

`tries/three.cljc`: `:str` → `:string`. Added assertions to actually test the string override and coll inheritance. Verified on both CLJ (all tries load) and CLJS (shadow-cljs compile main, 0 warnings).

### Active Open Threads (prioritized)

1. **`:tune` extension mode** — the middle ground between `:extend` and `:patch`. Needs type hierarchy awareness.
2. **Fork + mode inheritance** — should forks inherit parent's `:mode`? Probably not.
3. **Predicate guards** — runtime dispatch for non-class types (from state-bookkeeping-needs)
