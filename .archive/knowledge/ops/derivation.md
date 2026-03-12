# Derivation — Why This Vault Is Configured This Way

## Context

Pierre is building **nemesis**, a Clojure/ClojureScript cross-platform polymorphism library. The project has been in development for several years with periods of active work and dormancy. There's a WIP refactoring (`poly`) that represents a rethinking of the internals. The vault exists to make it possible to **resume development efficiently** — to pick up threads, remember decisions, and think through design problems with a partner.

## Derivation Rationale

### Granularity: Atomic
Nemesis is a macro-heavy library where small decisions have cascading implications. "Should cases be prepended or appended?" changes macro expansion behavior, which changes precedence semantics, which changes the user-facing API contract. Atomic notes let us trace these chains precisely. One decision, one note, clear links.

### Navigation: 2-Tier (Maps + Notes)
The project has clear subsystem boundaries (types, generics, extension, poly, CLJS) that serve as natural map categories. Flat would lose structure; 3-tier would over-organize a focused single-library project.

### Processing Depth: Deep
Pierre explicitly wants a thinking partner, not a filing cabinet. Every captured idea should be examined for implications, connections, and tensions before being stored. This is research-grade knowledge work.

### Self-Space: Enabled
The agent needs to build and maintain a model of the project. Understanding deepens over time — the agent should track what it's learned, what patterns work, and what the current goals are.

### Personality: Warm/Casual, Opinionated
Pierre wants pushback and engagement. The agent should feel like a collaborator who knows the codebase, not a secretary. Casual tone matches Clojure community culture and Pierre's communication style.

### Pipeline Chaining: Suggested
After capturing something, the agent suggests next steps (connect, update maps, flag tensions) but doesn't auto-run. Pierre stays in control of the workflow but gets prompted to do the linking work that makes the vault valuable.

### Selectivity: Moderate
Not everything said in a session is worth a note. But this is deep work — err toward capturing rather than dropping. If in doubt, inbox it.

### Domain: Single (Clojure Library Design)
Everything in this vault is about nemesis and its ecosystem. No need for multi-domain routing.
