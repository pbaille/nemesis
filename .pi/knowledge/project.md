# Project: thetis

## Identity
- **What**: Polymorphic generics library for Clojure and ClojureScript
- **Language**: Clojure (CLJ + CLJS via shadow-cljs)
- **Version**: 0.2.0
- **Distribution**: Git dependency (`io.github.pbaille/thetis`)
- **License**: MIT

## Vitals
- **Source lines**: ~2,500 (src), ~650 (tests), ~650 (tries/examples)
- **Dependencies**: `org.clojure/clojure 1.12.0`, `org.clojure/clojurescript 1.11.60`
- **Dev deps**: `djblue/portal 0.35.1`, `thheller/shadow-cljs 2.20.16`
- **Build**: deps.edn (no Leiningen)

## Entry Points
- **Main API**: `src/thetis/core.clj` — all public macros (`defg`, `generic+`, `fork`, `deft`, `thing`, `type+`, `defguard`)
- **CLJS stub**: `src/thetis/core.cljs` — runtime atoms only (`prototypes`, `guard-impls`)
- **Type queries**: `src/thetis/types.clj` — user-facing type hierarchy API

## Architecture Brief
Macro-driven system: `defg` parses user syntax → registers specs in compile-time state atom → generates protocols + `defn` wrappers + `extend` calls. Type keywords (`:vec`, `:map`, `:coll`) resolve to platform-specific classes at compile time. Runtime dispatch is pure protocol dispatch (zero overhead on CLJ). Guards add a two-phase pre-check via runtime atom before protocol dispatch.

Key architectural split:
- **Pure compiler** (`compiler/core.clj`): stateless transforms on compiler value maps
- **Impure state** (`state.clj`): global atom holding CLJ + CLJS registries side-by-side
- **Code generation** (`compiler/forms.clj`): emits `defprotocol`, `extend`, `defn`, JS prototype assignments

## Commands
```bash
# CLJ tests (via tries — assertion-based, not clojure.test)
clj -M -e "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"

# Unit tests (clojure.test — note: still use old 'poly' namespace)
clj -M -m clojure.test poly.types.core-test poly.functions.spec-test poly.incremental-build-test

# CLJS compilation + tests
npx shadow-cljs compile main

# Build jar
# target/thetis-0.2.0.jar exists
```

## Status
Stable macro layer. All extension modes, predicate guards, fork semantics, and CLJS support are implemented and tested.
