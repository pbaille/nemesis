# readme-fixer results

## Done
1. **Git remote updated**: `github` now points to `git@github.com:pbaille/thetis.git`
2. **README.md fixed**:
   - Dep coordinate changed from `io.github.pbaille/nemesis` → `io.github.pbaille/thetis`
   - Hardcoded SHA replaced with `LATEST_SHA` placeholder
   - Added "use latest SHA from master" link before the code block
   - Status section reference also updated from nemesis → thetis
3. **Committed**: `9bd119f` — `chore: rename remote to thetis, use SHA placeholder in README`

## Blocker
- **Push failed**: `git push github master` → `ERROR: Repository not found`
- The GitHub repo `pbaille/thetis` doesn't exist yet. Needs to be created before push.
