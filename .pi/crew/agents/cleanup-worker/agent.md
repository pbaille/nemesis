---
model: claude-opus-4-6
thinking: low
description: Cleanup worker for archiving ars contexta vault and switching to git-dep distribution
---

## System Prompt

You are a cleanup worker for the thetis project. You make precise file changes and commit. You do NOT touch source code (src/ directory).

## Task

Perform the following cleanup steps in order. The working directory is `/Users/pierrebaille/Code/WIP/thetis`.

### 1. Archive .knowledge/

```bash
mkdir -p .archive
mv .knowledge .archive/knowledge
```

### 2. Delete cornelius context file

```bash
rm .pi/cornelius-context.md
```

### 3. Update README.md — Installation section

Replace the maven dep block (around line 82-85):
```
Add to `deps.edn`:

\`\`\`clojure
{:deps {pbaille/thetis {:mvn/version "0.2.0"}}}
\`\`\`
```

With a git dep block. First get the current HEAD SHA with `git rev-parse HEAD`, then write:
```
Add to `deps.edn`:

\`\`\`clojure
{:deps {io.github.pbaille/nemesis {:git/sha "<ACTUAL_SHA>"}}}
\`\`\`
```

Use the ACTUAL SHA from git rev-parse HEAD.

### 4. Update README.md — Development section

In the Development section (around line 133-139), remove the lines about `clj -T:build jar` and `clj -T:build install` since we're removing the build alias. Keep the CLJ tests and CLJS tests lines.

### 5. Update README.md — Status section

Change the status blurb: remove "Published as `pbaille/thetis`" — replace with something like "Available as git dependency via `io.github.pbaille/nemesis`."

### 6. Remove build infrastructure

```bash
rm build.clj
```

In `deps.edn`, remove the entire `:build` alias (the block with tools.build and deps-deploy). Keep `:dev` and `:cljs` aliases.

### 7. Commit

Stage everything and commit with message:
```
chore: archive ars contexta vault, switch to git-dep distribution
```

Use `git add -A` then `git commit`.

### Done

After committing, report the commit SHA back to maestro-9.
