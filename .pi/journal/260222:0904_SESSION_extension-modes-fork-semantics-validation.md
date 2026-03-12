---
type: session
tags: [extension-modes, fork-semantics, mode-inheritance, type-hierarchy, poly-compiler]
status: complete
session: /Users/pierrebaille/.pi/agent/sessions/--Users-pierrebaille-Code-WIP-nemesis--/2026-02-22T07-02-28-265Z_a5c18a7c-fd9c-48e3-aa5b-20b6705a0940.jsonl
---

# Extension modes and fork semantics deep-dive

Implemented the four extension modes (:extend, :tune, :patch, :sealed) for the poly function system with mode-aware validation, then examined fork semantics to confirm mode should reset to :patch on fork. Explored concrete scenarios where forking makes sense (local variants, namespace isolation, prototype-based inheritance) and stress-tested each extension mode to assess redundancy and correctness.

## Key Points

- Implemented :tune mode with type-hierarchy-aware validation — allows specialization (adding :vec under :coll) but blocks direct overrides
- Tightened :extend mode to also block specialization via types/parents hierarchy check, making it meaningfully stricter than :tune
- :sealed mode prevents all extensions, signals 'fork me instead' pattern, enables base-generic-with-forked-variants design
- Fork should reset mode to :patch by default — allows sealed generics to be actually useful after forking (fork+ expands to fork then generic+)
- Confirmed fork+mode design is correct: :patch (permissive default), :extend (no specialization), :tune (specialize but not override), :sealed (no extensions)
- Identified tension: :extend blocks specialization for stability but can't be semantic (can't verify if new implementation is compatible) — structural check is correct tradeoff
- Debated but kept all four modes — none are redundant; they occupy distinct points on the safety/flexibility spectrum

## Files

- `src/poly/functions/spec.clj`
- `src/poly/functions/registry.clj`
- `src/poly/compiler/core.clj`
- `src/poly/tries/four.cljc`
- `.knowledge/notes/three-extension-modes.md`
- `.knowledge/notes/forking-semantics.md`
- `.knowledge/maps/open-threads-map.md`
- `.knowledge/self/goals.md`
