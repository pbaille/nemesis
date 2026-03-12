---
type: session
tags: [poly-refactoring, architecture, migration, registry-layer, namespace-fixes]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T12-19-34-521Z_7677bfb5-312d-43fc-a6f0-ae5c0ea94da2.jsonl
---

# Poly migration strategy and repository cleanup

After reviewing nemesis history and current state, agreed on a strangler fig migration to integrate the functional poly refactoring into the existing nemesis codebase. Fixed poly namespace/directory mismatches, wired nemesis.impl.registry to delegate to poly.functions equivalents, and began migrating nemesis.impl.forms to use poly.compiler.forms. All tests passing after registry layer migration.

## Key Points

- Created poly-migration branch for incremental refactoring
- Fixed poly namespace mismatch: renamed environment/ → compiler/ directory
- Fixed missing poly utility functions: name_derive, name_arify, binding-pattern_ensure-top-level-sym to poly.utils.names and poly.utils.misc
- Migrated nemesis.impl.registry to delegate to poly.functions.spec and poly.functions.registry, passing type registry explicitly
- Made poly.functions.spec/class-extensions accept optional class-resolver function to handle nemesis special cases (e.g., :default type)
- Began forms.clj migration: identified stateful vs pure functions; need to make poly.compiler.forms prototypes-sym configurable to emit nemesis.core/prototypes instead of poly.core/prototypes

## Files

- `src/poly/compiler/core.clj`
- `src/poly/compiler/data.clj`
- `src/poly/compiler/forms.clj`
- `src/poly/types/core.clj`
- `src/poly/types/data.clj`
- `src/poly/functions/spec.clj`
- `src/poly/functions/parse.clj`
- `src/poly/functions/registry.clj`
- `src/poly/utils/names.clj`
- `src/poly/utils/misc.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/impl/forms.clj`
