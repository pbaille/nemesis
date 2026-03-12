---
type: task
tags: [rename, namespace, packaging, thetis]
status: pending
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T11-25-33-527Z_2b57207c-7648-40c9-a537-8f03f9746e0a.jsonl
---

# Rename `poly` namespace to `thetis`

## Context

The library's internal namespace is currently `poly.*` but this name is too generic and collides with the Polylith ecosystem's mindshare. After evaluating shapeshifter names from mythology (Proteus — taken on Clojars, Mestra, Nereus, Thetis), the decision is **thetis** — the Greek sea nymph who shapeshifted to resist being constrained to a single form. Name is clear on Clojars (no conflicts). The artifact name stays `pbaille/nemesis` for Clojars continuity (the repo is `github.com/pbaille/nemesis`).

Summary: artifact = `pbaille/nemesis`, namespace = `thetis.*`, version = `0.2.0`.

## Scope

This is a mechanical rename: every occurrence of `poly.` (as namespace prefix) becomes `thetis.`, and the directory `src/poly/` becomes `src/thetis/`.

### Source files to move (20 files)

All under `src/poly/` → `src/thetis/`:

```
src/poly/compiler/core.clj    → src/thetis/compiler/core.clj
src/poly/compiler/data.clj    → src/thetis/compiler/data.clj
src/poly/compiler/forms.clj   → src/thetis/compiler/forms.clj
src/poly/core.clj             → src/thetis/core.clj
src/poly/core.cljs            → src/thetis/core.cljs
src/poly/functions/parse.clj  → src/thetis/functions/parse.clj
src/poly/functions/registry.clj → src/thetis/functions/registry.clj
src/poly/functions/spec.clj   → src/thetis/functions/spec.clj
src/poly/state.clj            → src/thetis/state.clj
src/poly/tries/one.cljc       → src/thetis/tries/one.cljc
src/poly/tries/two.cljc       → src/thetis/tries/two.cljc
src/poly/tries/three.cljc     → src/thetis/tries/three.cljc
src/poly/tries/four.cljc      → src/thetis/tries/four.cljc
src/poly/tries/five.cljc      → src/thetis/tries/five.cljc
src/poly/types.clj            → src/thetis/types.clj
src/poly/types/core.clj       → src/thetis/types/core.clj
src/poly/types/data.clj       → src/thetis/types/data.clj
src/poly/utils/expansion.clj  → src/thetis/utils/expansion.clj
src/poly/utils/misc.clj       → src/thetis/utils/misc.clj
src/poly/utils/names.clj      → src/thetis/utils/names.clj
```

### Namespace references inside source files (~83 occurrences)

Every `(ns poly.…)`, `(:require [poly.…])`, and string/symbol reference to `poly.` namespaces inside the 20 source files needs `poly.` → `thetis.`.

### Config files to update

- **`shadow-cljs.edn`** — `poly.core` → `thetis.core`, `poly.tries.*` → `thetis.tries.*`
- **`README.md`** — All `poly.core`, `poly.tries.*` references → `thetis.*`. Update the title from `# poly` to `# thetis`. Update the Quick Start require.
- **`build.clj`** — No namespace references to poly (uses `pbaille/nemesis` artifact). No changes needed.
- **`pom.xml`** — No namespace references. No changes needed.
- **`deps.edn`** — No namespace references. No changes needed.

### Vault/knowledge files

- `.knowledge/` notes reference `poly` extensively in design discussions. These are historical context and do NOT need renaming — they document the evolution. Optionally add a note about the rename decision.

## Task

### 1. Create `src/thetis/` directory tree
```bash
mkdir -p src/thetis/{compiler,functions,tries,types,utils}
```

### 2. Move all source files
Use `git mv` to preserve history:
```bash
git mv src/poly/compiler/core.clj src/thetis/compiler/core.clj
# ... etc for all 20 files
```

### 3. Find-and-replace `poly.` → `thetis.` in all source files
In every `.clj`, `.cljs`, `.cljc` under `src/thetis/`:
- `(ns poly.` → `(ns thetis.`
- `[poly.` → `[thetis.`  (in require forms)
- `'poly.` → `'thetis.` (in quoted symbols)
- `poly.core/` → `thetis.core/` (in qualified references)

Be careful with string literals that might contain `poly` in user-facing messages (e.g., error messages, println). Check each occurrence.

### 4. Update `shadow-cljs.edn`
- `:cache-blockers #{poly.core}` → `#{thetis.core}`
- `(poly.core/prepare-state!)` → `(thetis.core/prepare-state!)`
- entries: `poly.tries.*` → `thetis.tries.*`

### 5. Update `README.md`
- Title: `# poly` → `# thetis`
- All require examples: `[poly.core ...]` → `[thetis.core ...]`
- shadow-cljs config examples
- Test commands: `'poly.tries.one` → `'thetis.tries.one`
- Narrative text: "Poly replaces..." → "Thetis replaces..." etc.

### 6. Remove old `src/poly/` directory
After confirming everything works:
```bash
rm -rf src/poly/
```

### 7. Verify
```bash
# CLJ tests
clj -M -e "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"

# CLJS tests
npx shadow-cljs compile main

# JAR build
clj -T:build jar
```

All must pass. JAR contents should show `thetis/` namespace tree, not `poly/`.
