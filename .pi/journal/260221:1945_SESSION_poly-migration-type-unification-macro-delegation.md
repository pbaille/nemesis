---
type: session
tags: [poly-migration, type-system, macro-delegation, extension-modes, strangler-fig-pattern]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-21T17-29-59-782Z_e1b99b07-852b-4cad-a22c-ad10c694b97e.jsonl
---

# Poly migration: unify type handling and delegate macros

Implemented Steps 1-2 of the poly migration task: removed hardcoded :default/:nil branches from nemesis.types/classes to rely entirely on registry lookup, then delegated nemesis.impl.forms/thing_parse-impl-cases to poly.compiler.forms. Added initial scaffolding for extension modes (:extend/:tune/:patch) with :mode metadata support and mode validation in merge-cases. All tests pass.

## Key Points

- Step 1: Eliminated hardcoded :default/:nil branches in nemesis.types/classes—both are now resolved via registry lookup, unifying CLJ and CLJS paths
- Step 2: Delegated thing_parse-impl-cases to poly, with state-based name qualification to bridge nemesis mutable state to poly's functional model
- Step 3: Documented decision to keep *expansion-state* as a lightweight {env form} context—snaphotting the full compiler at macro entry would cause stale reads during macroexpansion
- Step 4 (in progress): Added :mode field to generic specs (defaulting to :patch for backward compat); implemented :extend mode check in merge-cases to prevent overrides; wrote test scaffolding but macro-time assertions require compile-time verification

## Files

- `src/nemesis/types.clj`
- `src/nemesis/impl/registry.clj`
- `src/nemesis/impl/forms.clj`
- `src/poly/functions/parse.clj`
- `src/poly/functions/spec.clj`
- `src/nemesis/tries/four.cljc`
- `.knowledge/notes/expansion-state-keep-as-is.md`
