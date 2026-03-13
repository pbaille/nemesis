# worker-comments — Result

## Task
Fix misleading comments about extension ordering in `src/thetis/functions/spec.clj`.

## Changes Made

**File:** `src/thetis/functions/spec.clj`

### 1. Comment block (around line 16–20)
- **Before:** `"most recently added wins"` + vague "sorted namespace keys (deterministic)"
- **After:** Clear statement that ordering is alphabetical by namespace name (descending), explicitly labelled deterministic and rebuild-safe, not temporal.

### 2. `effective-cases` docstring (line 26)
- **Before:** `"Returns cases in precedence order: latest extensions first, declaration last."`
- **After:** `"Returns cases in precedence order: alphabetically last namespace first, declaration last."`

## No other changes
Only comments and docstrings touched. No logic, no structure, no other files.
