# Learnings

## Test namespace mismatch — confirmed orphaned (high confidence)
Unit tests in `test/poly/` are fully orphaned: they use old `poly.*` namespaces, `test/` is not in deps.edn `:paths`, and no `poly.*` source exists. These files are dead artifacts from the poly→thetis rename. The `:test` alias only runs `thetis.tries.*`.

## Significant dead code in state.clj (high confidence)
Dead-code audit (2026-03-13) found 24 dead items across 8 files. Heaviest concentration in `state.clj` (5 dead vars), `compiler/` (5 dead vars), and `types.clj` (6+ unused re-exports). Key pattern: public API vars defined for completeness but never actually used by the macro system or user code. See `.pi/crew/results/dead-code-detective.md` for full report.

## Lazy sequences in macros (high confidence)
The `doall-rec` utility in `utils/misc.clj` exists because macro expansion uses dynamic vars (`*expansion-state*`). Lazy sequences generated during expansion would be realized later, outside the dynamic binding scope, causing incorrect compilation target detection. Every macro is wrapped with `doall-rec` via `defmac`.

## CLJS extend mechanism (high confidence)
CLJS doesn't have `extend` or `extend-type` that works like CLJ. Instead, `compiler/forms.clj` generates direct JavaScript prototype assignments via `js*`. Base types (`nil`, `string`, `number`, etc.) use `cljs.core/unchecked-set` instead. This is the trickiest code-gen area.

## Guard dispatch is open (medium confidence)
Guard impls are stored in a runtime atom, not compiled into protocols. This means guards added via `generic+` after the initial `defg` work correctly — the defn wrapper checks the atom on every call. The tradeoff is a small runtime cost for guard dispatch (atom deref + cond chain) vs zero-cost protocol dispatch for regular types.

## Extension case ordering is alphabetical, not temporal (medium confidence)
`functions.spec/effective-cases` orders extensions by reverse-alphabetical namespace string, not definition time. If two namespaces extend the same generic, the one later in the alphabet wins. Completely undocumented — a usability surprise for multi-namespace extension scenarios.

## Guard dispatch has runtime overhead (high confidence)
Despite README's "zero overhead" claim, guard dispatch uses atom deref + `some` over a vector on every call. The zero-overhead claim is only true for pure protocol dispatch (no `defguard` types). Once any guard case exists, every call pays the atom-deref cost.

## thetis.types namespace is undocumented public API (high confidence)
`thetis.types` exports hierarchy queries (`parents`, `childs`, `isa`, etc.) with a "User-facing" docstring, but none of it appears in README or API.md. Either document it or mark it internal.

## Namespace-keyed contribution model (high confidence)
Both type registry and function specs use namespace-keyed contribution tracking (`{ns-str → contributions}`). This is specifically designed for shadow-cljs incremental builds: when a namespace is recompiled, its old contributions are removed and re-registered, preventing duplication. The `prepared-namespaces` atom in `state.clj` ensures cleanup happens exactly once per ns per build pass.
