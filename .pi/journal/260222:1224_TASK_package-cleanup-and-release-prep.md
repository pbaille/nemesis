---
type: task
tags: [packaging, cleanup, release, pom, deps, archived-files]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T10-55-28-908Z_0b7c951e-6d2f-4c5e-ade0-027525eaac31.jsonl
---

# Package cleanup and release preparation

## Context

All design threads are resolved. All features are implemented and tested (CLJ + CLJS). A comprehensive `README.md` has been written. The project is feature-complete for a release.

However, the project still carries significant legacy cruft from the nemesis → poly migration:

1. **Stale `pom.xml`** — References `pbaille/nemesis` v0.1.2, Clojure 1.10.1, ClojureScript 1.10.764, and `github.com/univalence/nemesis`. None of this matches the current project (poly, Clojure 1.11.0-alpha4, ClojureScript 1.11.60).

2. **Legacy `.org` files** — Four `.org` files remain:
   - `README.org` (683 lines) — Old nemesis README, now superseded by `README.md`
   - `notes.org` (65 lines) — Archived, header says "migrated to .knowledge/"
   - `src/poly/notes.org` (13 lines) — Archived, header says "migrated to .knowledge/"
   - `src/poly/README.org` (3 lines) — Just "WIP refactoring of nemesis"

3. **No build tooling** — No `build.clj` for tools.build. The `deps.edn` has a `:release` alias referencing `applied-science/deps-library` but it's unclear if that still works or is the right approach.

4. **Dependency versions** — `deps.edn` uses `org.clojure/clojure {:mvn/version "1.11.0-alpha4"}`. Should this be a stable release for a library? ClojureScript version should also be reviewed.

5. **Namespace naming** — The library is `poly.core` but the project is still called "nemesis" in the repo. The `pom.xml` groupId/artifactId should reflect the actual library name.

## Files

### To clean up
- `pom.xml` — Stale Maven coordinates, wrong deps, wrong SCM URLs
- `README.org` — Superseded by `README.md`
- `notes.org` — Already archived
- `src/poly/notes.org` — Already archived
- `src/poly/README.org` — Already archived
- `deps.edn` — Review dependency versions, `:release` alias

### Reference
- `README.md` — The new documentation (just written)
- `deps.edn` — Current dependencies and aliases
- `shadow-cljs.edn` — CLJS build config (reference for CLJS dep version)
- `.knowledge/self/goals.md` — Lists packaging as next priority

## Task

### 1. Remove or archive legacy `.org` files

The content of these files has been migrated to `.knowledge/` and/or superseded by `README.md`. Options:
- **Delete** them (they're in git history if ever needed)
- **Move** them to a `doc/archived/` directory

Recommend: delete. They add noise and their content lives elsewhere.

### 2. Update `pom.xml` for poly

Update to reflect the current project:
- GroupId/artifactId — decide on coordinates (e.g., `pbaille/poly` or keep `pbaille/nemesis` for continuity?)
- Version — bump to `0.2.0` or `1.0.0` given the scope of changes
- Dependencies — match `deps.edn` (Clojure 1.11.x, ClojureScript 1.11.60)
- SCM URLs — update if repo has moved, or remove if not on GitHub yet

### 3. Review `deps.edn`

- Should the Clojure dep be a stable release (e.g., `1.12.0`) instead of `1.11.0-alpha4`?
- Is the `:release` alias with `applied-science/deps-library` still the intended publish mechanism?
- Consider adding a `build.clj` with tools.build if deps-library is abandoned

### 4. Decide on project/library naming

The repo directory is `nemesis`, the library namespace is `poly.core`, and the pom says `pbaille/nemesis`. This needs a decision:
- Rename the library to something distinct? (poly is a common name)
- Keep `nemesis` as the artifact name with `poly` as the internal ns?
- Rename fully to match?

This is a design decision — present options with tradeoffs, don't just pick one.

### 5. Verify everything still works after cleanup

After changes:
- `clj -e "(require 'poly.tries.one 'poly.tries.two 'poly.tries.three 'poly.tries.four 'poly.tries.five)"` — all CLJ tries pass
- `npx shadow-cljs compile main` — all CLJS tries compile and pass
- Ensure `deps.edn` paths are correct, no broken requires
