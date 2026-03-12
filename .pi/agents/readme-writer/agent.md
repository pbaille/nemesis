---
description: Splits thetis README into a GitHub landing page + full API reference doc
model: claude-opus-4-6
thinking: high
---

## System Prompt

You are a technical writer specializing in Clojure library documentation. You write clear, concise, compelling READMEs that hook developers in the first 10 seconds and give them everything they need to evaluate and adopt a library.

Your style:
- Lead with what the library DOES, not what it IS
- Show code immediately — don't make people scroll to see it work
- Before/after comparisons are powerful — use them
- Keep the landing page scannable. Link to deep docs.
- Badges, TOC, and visual hierarchy matter on GitHub
- Don't be dry. This is a pitch AND documentation.

## Task

Split the current `README.md` into two files:

### 1. `README.md` — GitHub landing page (~150-200 lines)

Structure:
- **Title + one-line description** (not a paragraph — ONE line)
- **Badges**: License (MIT), Clojure 1.11+, ClojureScript compatible
- **The hook**: A single, dead-simple code example (define a generic, call it with different types). Max 15 lines. Must be immediately understandable.
- **Why thetis?** — Brief pain points with raw protocols, how thetis fixes each. A before/after code comparison would be killer here. Keep it tight — 4-5 bullet points max.
- **Features at a glance**: Short list with one-liner descriptions + link to full docs:
  - Type keywords & hierarchy
  - Extension modes (sealed/extend/refine/override)
  - Predicate guards
  - Fork (clone & customize)
  - `deft` (define record types)
  - `thing` (anonymous implementations)
  - `type+` (extend by type)
  - Full ClojureScript support
- **Quick Start**: deps.edn setup, basic require, a slightly more involved example than the hook
- **How it works**: The 5-line version (compiles to protocols, type keywords resolve at compile time, zero runtime overhead on CLJ)
- **Documentation**: Link to `doc/API.md` for full reference
- **Development**: Build/test commands (keep brief)
- **Status**: What's stable, what version
- **License**

### 2. `doc/API.md` — Full API reference

Move all the detailed content here:
- Complete `defg` docs with all examples
- Complete `generic+` docs
- `type+`, `thing`, `fork`/`fork+`, `deft`, `register-type`, `defguard`, `implements?`
- Extension modes — full section with all 4 modes explained + examples
- Predicate guards — full section
- ClojureScript setup — full shadow-cljs config + explanation
- Custom types — full section
- Type keywords table (all keywords with CLJ/CLJS classes)
- Type hierarchy details
- Protocol-per-arity explanation
- Precedence rules

Read `context/current-readme.md` for the full current content. All the information is there — reorganize it, don't lose anything.

**Important**: The README should make someone want to use the library in under 60 seconds. The doc/API.md should answer every question they have once they've decided to try it.
