# Git Workflow for WRF-SUEWS

This document explains the git remote configuration and common workflows.

## Remote Configuration

```bash
origin   → git@github.com:sunt05/WRF-SUEWS.git (your fork)
upstream → git@github.com:Urban-Meteorology-Reading/WRF-SUEWS.git (original)
```

### What This Means:

- **origin**: Your personal fork where you push your changes
- **upstream**: The original project repository for pulling updates

## Current Branch

```
sunt05/update-wrf-suews-2025
```

**Contains:**
- WRF v4.7.1 update
- SUEWS 2025.10.15 update
- Coupling automator modernization
- Testing resources

## Common Workflows

### 1. Push Your Changes to Your Fork

```bash
# Push current branch to your fork
git push origin sunt05/update-wrf-suews-2025

# Or set upstream for easier future pushes
git push -u origin sunt05/update-wrf-suews-2025

# Then just use:
git push
```

### 2. Create a Pull Request

After pushing, create PR from your fork to the original repository:

```bash
# Using GitHub CLI
gh pr create --web

# Or manually at:
# https://github.com/sunt05/WRF-SUEWS/compare/sunt05/update-wrf-suews-2025?expand=1
```

### 3. Keep Your Fork Updated

```bash
# Fetch latest from original repository
git fetch upstream

# Update your master branch
git checkout master
git merge upstream/master
git push origin master

# Rebase your feature branch (optional)
git checkout sunt05/update-wrf-suews-2025
git rebase upstream/master
```

### 4. Check Remote Configuration

```bash
# View remotes
git remote -v

# View branch tracking
git branch -vv
```

### 5. Push to Different Remote

```bash
# Push to your fork (default)
git push origin sunt05/update-wrf-suews-2025

# Push to upstream (if you have permissions)
git push upstream sunt05/update-wrf-suews-2025
```

## Submodule Remotes

The submodules (WRF and SUEWS) have their own remotes:

```bash
# Check WRF remote
cd WRF
git remote -v
# origin → git@github.com:wrf-model/WRF.git

# Check SUEWS remote
cd ../SUEWS
git remote -v
# origin → git@github.com:UMEP-dev/SUEWS.git
```

These point to the official repositories and should generally not be changed.

## Branch Structure

```
master (tracking origin/master)
  └─ sunt05/update-wrf-suews-2025 (your feature branch)
```

## Typical Development Workflow

### Starting New Feature

```bash
# Make sure master is up to date
git checkout master
git pull origin master

# Create feature branch
git checkout -b sunt05/new-feature

# Make changes, commit
git add .
git commit -m "Description"

# Push to your fork
git push -u origin sunt05/new-feature
```

### Updating Existing Branch

```bash
# On your feature branch
git checkout sunt05/update-wrf-suews-2025

# Make changes
git add .
git commit -m "Description"

# Push to your fork
git push origin sunt05/update-wrf-suews-2025
```

### Syncing with Upstream

```bash
# Fetch upstream changes
git fetch upstream

# Option 1: Merge (creates merge commit)
git merge upstream/master

# Option 2: Rebase (cleaner history)
git rebase upstream/master

# Push (may need --force-with-lease if rebased)
git push origin sunt05/update-wrf-suews-2025
```

## Current Status

### Ready to Push

Your branch `sunt05/update-wrf-suews-2025` contains:

1. ✅ WRF v4.7.1 and SUEWS 2025.10.15 updates
2. ✅ Coupling automator modernization
3. ✅ Comprehensive testing resources
4. ✅ Mac Apple Silicon support
5. ✅ Updated documentation

**To push to your fork:**

```bash
git push origin sunt05/update-wrf-suews-2025
```

**To create a PR to the original repository:**

```bash
# Using GitHub CLI
gh pr create \
  --title "Update WRF to v4.7.1 and SUEWS to 2025.10.15" \
  --body "$(cat <<'EOF'
## Summary

Updates WRF-SUEWS to use the latest stable versions:
- WRF v4.0.2 → v4.7.1
- SUEWS → 2025.10.15 (from UMEP-dev repository)

## Changes

### Version Updates
- Updated WRF submodule to v4.7.1
- Updated SUEWS submodule to 2025.10.15 from UMEP-dev
- Changed SUEWS remote from Urban-Meteorology-Reading to UMEP-dev

### Coupling Modernization
- Updated `automate_main.py` for new SUEWS structure (`src/suews`)
- Modernized `gen_suewsdrv.py` to parse new Makefile format
- Added handling for auto-generated version module
- Verified compatibility with both new versions

### Testing Resources
- Added `test_coupling.sh` - automated verification script
- Added `TESTING.md` - comprehensive testing guide (5 levels)
- Added `QUICKSTART.md` - quick start guide for users
- All tests passing on macOS (Apple Silicon)

### Documentation
- Updated README with current versions and Mac support
- Enhanced Apple Silicon compilation guide
- Added notes about repository migrations

## Testing

✅ Coupling automator generates 50,412-line SUEWS driver
✅ All 7 WRF files properly modified
✅ 84 SUEWS state variables registered
✅ Compatible with WRF v4.7.1 and SUEWS 2025.10.15
✅ Automated test script confirms all checks passed

Ready for compilation testing on target platforms.

## Platforms

- ✅ macOS (Apple Silicon) - Tested, ready for compilation
- ⏳ JASMIN - Needs testing
- ⏳ Linux - Needs testing
EOF
)" \
  --base master

# Or manually create PR at GitHub web interface
```

## Tips

### View Your Fork on GitHub

```bash
gh repo view sunt05/WRF-SUEWS --web
```

### View Original Repository

```bash
gh repo view Urban-Meteorology-Reading/WRF-SUEWS --web
```

### Compare Your Changes

```bash
# Compare with upstream master
git diff upstream/master..sunt05/update-wrf-suews-2025

# View commit log
git log upstream/master..sunt05/update-wrf-suews-2025 --oneline
```

### Check What Will Be Pushed

```bash
# See what commits will be pushed
git log origin/sunt05/update-wrf-suews-2025..sunt05/update-wrf-suews-2025

# If branch doesn't exist on origin yet:
git log master..sunt05/update-wrf-suews-2025
```

## Troubleshooting

### Problem: "remote origin already exists"

```bash
# Remove old remote
git remote remove origin

# Add new remote
git remote add origin git@github.com:sunt05/WRF-SUEWS.git
```

### Problem: "failed to push some refs"

```bash
# Fetch first
git fetch origin

# Then rebase (if needed)
git rebase origin/sunt05/update-wrf-suews-2025

# Force push (use with caution)
git push --force-with-lease origin sunt05/update-wrf-suews-2025
```

### Problem: "Your branch and 'origin/master' have diverged"

```bash
# Option 1: Keep your changes on top
git rebase origin/master

# Option 2: Merge
git merge origin/master
```

## References

- **Your Fork**: https://github.com/sunt05/WRF-SUEWS
- **Original**: https://github.com/Urban-Meteorology-Reading/WRF-SUEWS
- **WRF Official**: https://github.com/wrf-model/WRF
- **SUEWS (UMEP-dev)**: https://github.com/UMEP-dev/SUEWS
