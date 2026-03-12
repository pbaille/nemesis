# readme-writer — Result

## What I Did

Split the monolithic `README.md` (330+ lines) into two focused files:

### 1. `README.md` — GitHub Landing Page (148 lines)
- **One-line description** that hooks immediately
- **Badges**: MIT license, Clojure 1.11+, ClojureScript compatible
- **Hook example**: 15-line `defg` + `generic+` — instantly understandable
- **Why thetis?**: Before/after HTML table comparing raw protocols vs thetis, plus 5 bullet points
- **Features table**: 8 features, each with one-liner + deep link to `doc/API.md`
- **Quick Start**: deps.edn + richer example (combine, generic+, fork)
- **How it works**: 5-line summary
- **Documentation link**: Points to full API reference
- **Development**: Build/test commands (4 lines)
- **Status**: v0.2.0, stable
- **License**: MIT

### 2. `doc/API.md` — Full API Reference (571 lines)
All detailed content moved here with improved organization:
- **Core Concepts**: Type keywords (full table), type hierarchy (table format), precedence rules, protocol-per-arity explanation
- **API**: Complete docs for all 9 macros/functions: `defg`, `generic+`, `type+`, `thing`, `fork`/`fork+`, `deft`, `register-type`, `defguard`, `implements?`
- **Extension Modes**: All 4 modes with examples and ✓/✗ markers + fork mode reset
- **Predicate Guards**: Full section — defining, using, dispatch order, extension mode integration, guards across forks
- **ClojureScript**: shadow-cljs config with detailed explanation of each setting
- **Custom Types**: `deft` and `register-type` with full examples
- **How It Works**: Technical internals (5 points)
- Table of contents at the top, back-links to README

## What's Preserved
Every piece of information from the original README is present in one of the two files. Verified by checking all key terms appear in at least one file.

## File Sizes
- `README.md`: 148 lines / 5.1 KB — scannable in under 60 seconds
- `doc/API.md`: 571 lines / 17.5 KB — complete reference
