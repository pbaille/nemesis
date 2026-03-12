# worker-sha-fix result

## Task
Replace hardcoded SHA in README.md Quick Start section with `"LATEST"` placeholder.

## Change made
**File:** `README.md`, line 85

```diff
- {:deps {io.github.pbaille/thetis {:git/sha "453832d5e3c5bf7c0d4b85f94d8c348bf32f60f3"}}}
+ {:deps {io.github.pbaille/thetis {:git/sha "LATEST"}}}
```

Only that one line was changed. Nothing else was touched.
