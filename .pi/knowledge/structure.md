# Directory Structure

```
thetis/
├── deps.edn                    # Project deps (clojure, clojurescript)
├── shadow-cljs.edn             # CLJS build config with build hooks
├── pom.xml                     # Maven POM for jar publishing
├── README.md                   # Comprehensive user-facing docs
├── doc/
│   └── API.md                  # Full API reference
├── src/thetis/
│   ├── core.clj                # (214 lines) Public macro API — defg, generic+, fork, deft, thing, type+, defguard
│   ├── core.cljs               # (4 lines) CLJS runtime atoms (prototypes, guard-impls)
│   ├── state.clj               # (131 lines) Global compile-time state atom, expansion context
│   ├── types.clj               # (117 lines) User-facing type queries, predicate compilation
│   ├── types/
│   │   ├── core.clj            # (176 lines) Pure type registry operations, hierarchy, contributions
│   │   └── data.clj            # (60 lines) Platform type data (CLJ/CLJS base types, groups)
│   ├── compiler/
│   │   ├── core.clj            # (133 lines) Pure compiler operations (query + update)
│   │   ├── data.clj            # (20 lines) Compiler constructors, platform-initialized values
│   │   └── forms.clj           # (327 lines) Code generation — protocols, extend, CLJS prototype
│   ├── functions/
│   │   ├── parse.clj           # (153 lines) Parse defg/generic+ syntax into spec maps
│   │   ├── registry.clj        # (65 lines) Function spec registry CRUD
│   │   └── spec.clj            # (183 lines) Spec manipulation, extension modes, cloning
│   ├── utils/
│   │   ├── expansion.clj       # (25 lines) CLJ/CLJS detection, symbol resolution
│   │   ├── misc.clj            # (196 lines) Symbol utils, fn-case normalization, defmac
│   │   └── names.clj           # (24 lines) Protocol/method naming conventions
│   └── tries/                  # Integration tests (assertion-based, .cljc for cross-platform)
│       ├── one.cljc            # (229 lines) Core features: defg, generic+, type+, fork, deft, thing
│       ├── two.cljc            # (61 lines) Cross-ns deft, thing
│       ├── three.cljc          # (14 lines) Fork + override
│       ├── four.cljc           # (224 lines) Extension modes (sealed, extend, refine, override)
│       └── five.cljc           # (124 lines) Predicate guards (defguard)
├── test/poly/                  # Unit tests (clojure.test — STILL USE OLD 'poly' NAMESPACE)
│   ├── types/core_test.clj     # (130 lines) Type registry contributions, idempotent rebuild
│   ├── functions/spec_test.clj # (186 lines) Spec merge, extension ordering, clone isolation
│   └── incremental_build_test.clj # (297 lines) Full incremental build scenarios
├── target/                     # Build artifacts (jar, compiled classes)
├── public/                     # CLJS dev server output (shadow-cljs)
└── .archive/                   # Archived ars contexta knowledge vault
```
