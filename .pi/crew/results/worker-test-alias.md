# worker-test-alias — Result

## Task
Add `:test` alias to `deps.edn` and rewrite `## Development` section in `README.md`.

## Changes

### `deps.edn`
Added `:test` alias to `:aliases` map:
```clojure
:test {:main-opts ["-e" "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"]}
```

### `README.md`
Replaced verbose `## Development` section with clean two-liner:
```markdown
## Development

```bash
clj -M:test                      # Run CLJ test suite
npx shadow-cljs compile main     # Run CLJS tests
```
```

## Verification
`clj -M:test` ran successfully (exit 0). Output:
- One expected assertion error (from tries.four testing `:extend` mode enforcement)
- `Thetis Extension modes: ALL TESTS OK`
- `Thetis Predicate Guards: ALL TESTS OK`

No other files were touched.
