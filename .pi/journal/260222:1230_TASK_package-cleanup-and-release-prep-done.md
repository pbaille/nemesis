---
type: task
tags: [packaging, cleanup, release, pom, deps, build]
status: done
session: current
---

# Package cleanup and release preparation — DONE

## What was done

### 1. Legacy `.org` files — DELETED
- `README.org` (superseded by `README.md`)
- `notes.org` (already archived header)
- `src/poly/notes.org` (already archived header)
- `src/poly/README.org` (3-line stub)

These were never tracked in git — deletion is local cleanup only.

### 2. `pom.xml` — REWRITTEN
- GroupId/artifactId: `pbaille/nemesis` (kept for continuity)
- Version: `0.2.0`
- Clojure dep: `1.11.4` (minimum floor — stable 1.11.x)
- ClojureScript dep: `1.11.60` with `<scope>provided</scope>`
- SCM URLs: updated to `github.com/pbaille/nemesis`
- License: EPL-2.0
- Removed from `.gitignore` so it's now tracked

### 3. `deps.edn` — ALREADY UPDATED (previous session)
- Clojure: `1.12.0` (dev version, higher than pom minimum)
- ClojureScript: `1.11.60`
- `:build` alias with `tools.build` 0.10.7 + `deps-deploy` 0.2.2
- Removed old `:release` alias (was `applied-science/deps-library`)

### 4. `build.clj` — CLEANED UP
- Removed redundant `pom-data` (pom.xml template is source of truth)
- Tasks: `clean`, `jar`, `deploy` (Clojars), `install` (local .m2)
- All working — verified `jar` and `install` tasks

### 5. `release.edn` — DELETED
- Was config for old `applied-science/deps-library` — no longer needed

### 6. `README.md` — UPDATED
- Quick Start: proper Maven coordinates (`pbaille/nemesis {:mvn/version "0.2.0"}`) + local checkout option
- Added Development section with test/build/deploy commands

### 7. `.gitignore` — UPDATED
- Removed `pom.xml` from ignore list (now tracked)

## Naming decision
Kept `pbaille/nemesis` as artifact name. The namespace is `poly.core` internally.
Rationale: continuity with existing Clojars artifact, repo is already `nemesis` on GitHub.
Can rename in a future major version if desired.

## Verification
- ✅ CLJ tests: all 5 tries pass on Clojure 1.12.0
- ✅ CLJS tests: shadow-cljs compiles clean (1 expected redef warning)
- ✅ JAR build: `clj -T:build jar` produces clean 30KB JAR
- ✅ Local install: `clj -T:build install` works
- ✅ JAR contents: only `poly/` namespace tree, correct Maven metadata
