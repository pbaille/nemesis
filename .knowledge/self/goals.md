# Goals

## Immediate
- ~~Build comprehensive knowledge of the current nemesis codebase and its design rationale~~ ✅
- ~~Catalog all open threads from notes.org and TODO comments into trackable notes~~ ✅
- ~~Understand the poly refactoring vision and how far it's gotten~~ ✅

## Medium-Term
- ~~Help resolve the major open design questions (forking semantics, extension modes, case accumulation)~~ — All three resolved ✅
- ~~Support the poly refactoring with clear architectural notes~~ ✅
- ~~Ensure ClojureScript compatibility path is well-documented~~ ✅ — README covers shadow-cljs setup + build hooks

## Long-Term
- A clean, functional nemesis with full CLJS support
- Every major design decision documented with rationale and alternatives considered
- A vault that makes picking up this project after any dormancy period trivial

## Current Focus
- **All open design threads resolved** (2026-02-22) — see [[open-threads-map]]
- Thetis macro layer is complete and tested — all 5 CLJ tries pass
- **ClojureScript verified** — all 5 tries compile and pass under shadow-cljs
- Incremental builds work with `prepare-state!` hook
- ✅ All four extension modes: `:sealed`, `:extend`, `:refine` (default), `:override`
- ✅ Fork semantics: full structural clone, mode reset to `:override`, isolation verified
- ✅ Predicate guards: `defguard` macro, two-phase dispatch, extension mode integration
- ✅ Compile-time prototype mirror: closed as unnecessary — compile-time spec is authoritative
- ✅ README.md written — full API reference, extension modes, guards, CLJS setup
- ✅ Namespace rename: `poly.*` → `thetis.*` (artifact stays `pbaille/nemesis`)

## Next Priorities
- ~~Packaging for distribution (Maven/Clojars coordinates, version)~~ ✅ — `pbaille/nemesis` 0.2.0, `build.clj` with tools.build
- ~~Clean up old `README.org` and `notes.org` (archived material)~~ ✅ — deleted, `release.edn` removed
- Consider parked threads if time allows: [[runtime-state-facilities]], [[defg-as-defn]]
- Performance benchmarks against raw protocols
- More real-world usage examples

## Parked
- Deploy to Clojars — not a priority for now. When ready: set `CLOJARS_USERNAME` / `CLOJARS_PASSWORD`, run `clj -T:build deploy`
