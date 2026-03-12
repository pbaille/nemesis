# Learnings

## Test namespace mismatch (high confidence)
Unit tests in `test/poly/` still use old `poly.*` namespaces (`poly.types.core`, `poly.functions.spec`, `poly.compiler.core`, etc.) from before the poly→thetis rename. The source code has been renamed to `thetis.*` but the test imports haven't been updated. These tests likely cannot run as-is unless there's a classpath trick or the `poly` namespaces still exist in `target/`.

## Lazy sequences in macros (high confidence)
The `doall-rec` utility in `utils/misc.clj` exists because macro expansion uses dynamic vars (`*expansion-state*`). Lazy sequences generated during expansion would be realized later, outside the dynamic binding scope, causing incorrect compilation target detection. Every macro is wrapped with `doall-rec` via `defmac`.

## CLJS extend mechanism (high confidence)
CLJS doesn't have `extend` or `extend-type` that works like CLJ. Instead, `compiler/forms.clj` generates direct JavaScript prototype assignments via `js*`. Base types (`nil`, `string`, `number`, etc.) use `cljs.core/unchecked-set` instead. This is the trickiest code-gen area.

## Guard dispatch is open (medium confidence)
Guard impls are stored in a runtime atom, not compiled into protocols. This means guards added via `generic+` after the initial `defg` work correctly — the defn wrapper checks the atom on every call. The tradeoff is a small runtime cost for guard dispatch (atom deref + cond chain) vs zero-cost protocol dispatch for regular types.

## Namespace-keyed contribution model (high confidence)
Both type registry and function specs use namespace-keyed contribution tracking (`{ns-str → contributions}`). This is specifically designed for shadow-cljs incremental builds: when a namespace is recompiled, its old contributions are removed and re-registered, preventing duplication. The `prepared-namespaces` atom in `state.clj` ensures cleanup happens exactly once per ns per build pass.
