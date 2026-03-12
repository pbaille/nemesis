---
type: task
tags: [forking, open-threads, housekeeping, vault-update]
status: done
parent: 260222:0910_TASK_close-fork-semantics-and-housekeep-open-threads.md
---

# Close forking-semantics thread + housekeep open threads map — DONE

## What Changed

### 1. `forking-semantics.md` → resolved
- Frontmatter: `type: open-thread` → `type: decision`, `status: active` → `status: resolved`, added `resolved: 2026-02-22`
- Each of the 3 remaining sub-questions got a **Resolution** annotation:
  - Runtime state cloning — NOT A PROBLEM (new protocols = full isolation by construction)
  - Structural sharing — LOW PRIORITY, NON-BLOCKING (Clojure persistent structures already share internally)
  - Fork of a fork — ALREADY WORKS (tested in `tries/three.cljc`)
- Stale "Design Space" section replaced with **Resolution Summary** that confirms all 4 sub-questions are closed
- The note now reads as a complete decision record, not an open thread

### 2. `open-threads-map.md` updated
- Removed `[[forking-semantics]]` from Active → Extension System section (section removed since it was the only entry)
- Added `[[forking-semantics]]` to Resolved section with full summary
- Updated By Subsystem → Extension: `(active)` → `(resolved)`

### 3. `goals.md` updated
- Medium-term: struck through "Help resolve the major open design questions (forking semantics, extension modes, case accumulation)" — all three resolved ✅
- Current Focus: added ✅ for fork semantics, updated "Next:" to point at predicate guards from `[[state-bookkeeping-needs]]`

### 4. Vault consistency verified
- Grepped `.knowledge/` for stale "active"/"open" references to forking — none found
- Cross-references in `related:` frontmatter of other notes are just links, don't claim status — fine as-is

## Remaining Active Threads
Only two remain:
1. `[[state-bookkeeping-needs]]` — predicate guards + compile-time prototype mirror
2. `[[poly-compiler-as-value]]` — ongoing architectural pattern, not blocking
