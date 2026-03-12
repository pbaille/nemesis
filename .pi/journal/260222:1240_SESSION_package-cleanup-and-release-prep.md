---
type: session
tags: [release-preparation, maven-build, packaging, tools-build, naming-decision]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T11-25-33-527Z_2b57207c-7648-40c9-a537-8f03f9746e0a.jsonl
---

# Package cleanup and release prep completed

Completed release readiness for nemesis: deleted legacy .org files, rewrote pom.xml with correct Maven coordinates and dependencies, verified build tooling (tools.build) works cleanly, updated README with proper Maven install instructions and development section. Kept artifact name as pbaille/nemesis (namespace remains poly.core) for backward compatibility. All tests pass on Clojure 1.12.0 and ClojureScript 1.11.60, JAR builds cleanly with no legacy artifacts.

## Key Points

- Deleted 4 legacy .org files (README.org, notes.org, src/poly/notes.org, src/poly/README.org) and release.edn
- Rewrote pom.xml with pbaille/nemesis coordinates, Clojure 1.11.4 floor, CLJS 1.11.60 (provided scope), EPL-2.0 license
- Verified build.clj with tools.build works: jar, install, deploy tasks all functional
- Removed pom.xml from .gitignore to track it as source of truth for Maven metadata
- Updated README with Maven coordinates and development/build section
- Confirmed all 5 CLJ tries pass on Clojure 1.12.0, CLJS compiles under shadow-cljs 2.20.16
- Naming decision deferred: user wants to explore shapeshifter-themed alternatives beyond 'poly' (too generic, Polylith conflict)

## Files

- `pom.xml`
- `deps.edn`
- `build.clj`
- `.gitignore`
- `README.md`
- `release.edn`
