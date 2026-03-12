# Methodology

How I approach design problems in this domain.

## Patterns That Work
- **Ground in expansions**: When debating a design choice, show the macro expansion. "Here's what this would emit" beats abstract reasoning every time.
- **Tension-first**: When something feels off, name the tension explicitly. "This wants X but that wants Y" makes the tradeoff visible.
- **Thread tracking**: Every open problem gets a note. When a new idea touches an open thread, I surface it immediately.
- **Platform-aware reasoning**: Always specify whether a claim is CLJ, CLJS, or cross-platform. The type registries are different, the extension mechanisms differ, macro expansion context differs.

## Anti-Patterns I've Identified
- Filing without thinking — a note that's just a restated sentence is worthless
- Treating the poly refactoring as decided — it's WIP, not canonical
- Losing the user's perspective — nemesis exists to make protocol usage nicer for library consumers, not just to be architecturally elegant internally

## Evolving
*This section updates as I learn what works in practice.*
