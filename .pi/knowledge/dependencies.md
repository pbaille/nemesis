# Dependencies

## Runtime
| Dependency | Version | Role |
|-----------|---------|------|
| `org.clojure/clojure` | 1.12.0 | Core language runtime |
| `org.clojure/clojurescript` | 1.11.60 | CLJS compilation (used for `cljs.analyzer` at compile time) |

## Dev-only
| Dependency | Version | Role |
|-----------|---------|------|
| `djblue/portal` | 0.35.1 | Dev inspection tool (via `:dev` alias) |
| `thheller/shadow-cljs` | 2.20.16 | CLJS build tool (via `:cljs` alias) |

## Notes
- Extremely minimal dependency footprint — only Clojure and ClojureScript
- `cljs.analyzer` is required at compile time in `utils/expansion.clj` for CLJS symbol resolution
- No runtime dependencies beyond Clojure itself
- Portal is optional dev tooling (tap> calls scattered in code)
