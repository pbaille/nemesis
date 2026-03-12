---
type: task
tags: [forking, open-threads, housekeeping, vault-update]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T08-04-09-764Z_60f25006-f1d5-4dac-8c1f-f4af750728af.jsonl
---

# Close forking-semantics thread + housekeep open threads map

## Context

After completing the extension modes rename (`:tune`→`:refine`, `:patch`→`:override`) and implementing fork mode reset to `:override`, we reviewed the remaining open sub-questions in `forking-semantics.md` and concluded they're all non-issues:

1. **Runtime state cloning** — Fork generates completely new protocol interfaces. Prototypes atom entries are keyed by the *new* generic name. Old and new are fully independent at JVM/JS level. Not a problem in practice.

2. **Structural sharing vs full clone** — Clojure persistent data structures already share structure. The explicit "low priority optimization" label in the note is correct. Non-blocking.

3. **Fork of a fork** — Already works and is tested. `tries/three.cljc` forks `two/g1` which is itself a fork of `one/g1`. `:cloned-from` tracks immediate parent, which suffices.

4. **Fork + extension modes** — ✅ Already resolved. Fork resets to `:override`.

The "Design Space" section at the bottom of the note still says "the biggest real question is fork + extension modes" — this is stale.

After closing this, the only active threads remaining are:
- `state-bookkeeping-needs` (predicate guards section)
- `poly-compiler-as-value` (ongoing architectural pattern, not blocking)

The `goals.md` "Next:" line also needs updating since fork mode inheritance is done.

## Files

- `.knowledge/notes/forking-semantics.md` — mark as resolved, update stale sections
- `.knowledge/maps/open-threads-map.md` — move forking-semantics from Active to Resolved, update summary
- `.knowledge/self/goals.md` — update "Next:" line now that fork is resolved

## Task

### 1. Update `forking-semantics.md`

- Change frontmatter `status: active` → `status: resolved`
- Add a "Resolution" section summarizing why all sub-questions are settled
- Update the stale "Design Space" section that still calls fork+modes the biggest question
- Each of the three remaining "What's Unresolved" items should get a brief resolution note (non-issue / already tested / low-priority optimization not worth tracking)

### 2. Update `open-threads-map.md`

- Move `[[forking-semantics]]` from Active to Resolved with a summary like: "Full structural clone, fork resets to `:override`, isolation tested across fork chains. All sub-questions assessed as non-blocking."
- Remove the Extension System section from Active if forking-semantics was its only entry
- Update the By Subsystem → Extension section

### 3. Update `goals.md`

- Change the "Next:" line from "resolve fork + mode inheritance question, or tackle predicate guards" to reflect that the next open design thread is predicate guards (from `state-bookkeeping-needs`)
- Add a checkmark for fork semantics being resolved

### 4. Verify vault consistency

- Grep `.knowledge/` for any remaining references to forking-semantics as "active" or "open"
- Ensure all cross-links are consistent
