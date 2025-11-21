# Continuous Integration and Testing

## Overview

WRF-SUEWS uses GitHub Actions for automated testing of the coupling system. This ensures that changes to the coupling automation scripts, WRF modifications, or SUEWS library integration don't break the build process.

## Workflow: `test-coupling.yml`

**Location**: `.github/workflows/test-coupling.yml`

**Triggers**:
- Push to `master`, `develop`, or any `sunt05/*` branches
- Pull requests to `master` or `develop`
- Manual workflow dispatch

**Platform**: Ubuntu 22.04 with GCC 11

### Workflow Stages

The workflow consists of 5 dependent jobs that test different aspects of the coupling:

#### 1. Build SUEWS Library
**Purpose**: Verify that SUEWS can be compiled as a static library

**Steps**:
- Install Fortran compiler and NetCDF libraries
- Build SUEWS using `Makefile.lib`
- Verify `libsuews.a` and module files exist
- Cache library artifacts for downstream jobs

**Validates**:
- SUEWS source code compiles cleanly
- Library build system (`Makefile.lib`) works correctly
- All dependencies are satisfied

**Outputs**: Cached `SUEWS/lib/` and `SUEWS/include/` directories

---

#### 2. Test Coupling Automation
**Purpose**: Verify the Python automation scripts run successfully

**Steps**:
- Restore SUEWS library from cache
- Install Python dependencies
- Run `automate_main.py`
- Verify compilation directory created with correct structure
- Check that WRF source files were copied
- Verify SUEWS wrapper files are present
- Confirm WRF Registry files were modified

**Validates**:
- `automate_main.py` executes without errors
- `changes_list.json` modifications apply correctly
- File copying and directory structure creation works
- Registry modifications are inserted properly

**Outputs**: None (compilation directory recreated in downstream jobs)

---

#### 3. Test WRF Configuration
**Purpose**: Verify WRF's configure script runs and can be patched

**Steps**:
- Restore SUEWS library from cache
- Re-run coupling automation to create compilation directory
- Install WRF build dependencies (NetCDF, compilers)
- Run `./configure` (option 34: Linux x86_64 dmpar)
- Apply SUEWS library patches using `patch_configure.py`
- Verify `configure.wrf` contains SUEWS flags

**Validates**:
- WRF configure script works with coupled source
- `inject_suews_link_flags()` function correctly patches `configure.wrf`
- SUEWS library paths and flags are properly injected
- No conflicts between WRF and SUEWS build systems

**Outputs**: None (compilation directory recreated in next job)

---

#### 4. Test WRF Registry Processing
**Purpose**: Verify WRF's code generation from Registry files

**Steps**:
- Restore SUEWS library from cache
- Re-run coupling automation to create compilation directory
- Install WRF dependencies
- Run `./configure` and patch with SUEWS flags
- Run `make registry` to generate code
- Verify generated files exist
- Check for SUEWS variables in generated code

**Validates**:
- `registry.suews` syntax is correct
- WRF's Registry parser accepts SUEWS additions
- Code generation includes SUEWS state variables
- No conflicts in variable namespaces

**Why This Matters**: Registry processing is the most fragile part of WRF compilation. This catches syntax errors or namespace conflicts early.

---

#### 5. Test Summary
**Purpose**: Report overall pass/fail status

**Outputs**: Summary of all job results

---

## What's NOT Tested (Yet)

The current workflow stops at Registry processing. **Full WRF compilation is NOT performed** due to:

1. **Time constraints**: Full WRF compilation takes 30-60 minutes
2. **Resource limits**: GitHub Actions free tier has limited compute hours
3. **Memory requirements**: WRF compilation can exceed runner memory

### Future Enhancements

Potential additions to the testing suite:

#### Full Compilation Test (Optional)
```yaml
test-full-compile:
  runs-on: ubuntu-22.04
  timeout-minutes: 90
  steps:
    - name: Compile WRF-SUEWS
      run: ./compile em_real >& log.compile
```

**Pros**: Catches linker errors and missing symbols
**Cons**: Expensive in time and compute resources

#### Runtime Smoke Test
```yaml
test-runtime:
  steps:
    - name: Run minimal simulation
      run: |
        cd test/em_real
        ./ideal.exe
        ./wrf.exe
```

**Pros**: Validates actual coupling behaviour
**Cons**: Requires input files, takes significant time

#### Matrix Testing
```yaml
strategy:
  matrix:
    os: [ubuntu-20.04, ubuntu-22.04, ubuntu-24.04]
    compiler: [gcc-10, gcc-11, gcc-12, intel]
```

**Pros**: Tests platform compatibility
**Cons**: Multiplies resource usage

---

## Using the Workflow

### Viewing Test Results

1. **On GitHub**:
   - Navigate to repository → Actions tab
   - Click on workflow run to see job results
   - View detailed logs for each job step

2. **Local Reproduction**:
   ```bash
   # Simulate the workflow locally
   docker run -it ubuntu:22.04
   apt-get update && apt-get install -y gfortran gcc g++ make libnetcdf-dev
   # ... follow workflow steps
   ```

### Debugging Failures

**Library Build Failure**:
- Check SUEWS submodule commit (should be 2025.10.15)
- Verify Makefile.lib exists in `SUEWS/src/suews/`
- Check for missing NetCDF or Fortran compiler

**Coupling Automation Failure**:
- Review `automate_main.py` output in job logs
- Check if `changes_list.json` syntax is valid
- Verify submodule paths are correct

**Configure Patch Failure**:
- Examine `configure.wrf` structure (may have changed in new WRF versions)
- Check `inject_suews_link_flags()` regex patterns
- Verify SUEWS library paths in `wrf_suews.mk`

**Registry Processing Failure**:
- Review `registry.suews` syntax
- Check for namespace conflicts with existing WRF variables
- Look for missing `use` statements in generated code

---

## Performance Considerations

### Cache Strategy

The workflow uses GitHub Actions cache for the SUEWS library:

```yaml
uses: actions/cache/save@v4
with:
  path: SUEWS/lib/
  key: suews-lib-${{ github.sha }}
```

**Benefits**:
- Library built once, reused in all downstream jobs
- Saves ~5 minutes per job (3 jobs = 15 minutes saved)
- Reduces total workflow time by 50%

**Cache Key**: Based on git commit SHA, ensuring freshness

**No Artifact Storage**: The workflow deliberately avoids uploading compilation directories as artifacts. Instead, each job recreates the compilation directory from source (~2 seconds with Python). This:
- Reduces storage costs (no 500MB+ artifacts)
- Keeps workflow simple (no artifact management)
- Ensures each stage tests the coupling process independently

### Parallel vs Sequential

Jobs run **sequentially with dependencies** (not parallel):
- Prevents wasted compute if early stage fails
- Shares cached SUEWS library across all jobs
- Easier debugging (clear failure point)
- Each job independently rebuilds from source (fast with Python automation)

If full compilation is added, consider:
```yaml
strategy:
  fail-fast: false  # Continue other jobs if one fails
```

---

## Workflow Maintenance

### When to Update

**WRF Version Changes**:
- Update configure option (currently option 34 for dmpar)
- May need new dependencies

**SUEWS Library Changes**:
- Update cache key if library build changes
- Verify `Makefile.lib` still exists

**Platform Updates**:
- Test on newer Ubuntu versions
- Update GCC version as needed

### Cost Management

GitHub Actions free tier (as of 2024):
- 2,000 minutes/month for private repos
- Unlimited for public repos

Current workflow uses ~15 minutes per run. For a public repository, this is sustainable.

---

## Integration with Development Workflow

### Pre-commit Hooks

Consider adding local pre-commit validation:

```bash
# .git/hooks/pre-commit
#!/bin/bash
python3 coupling-automator/automate_main.py --dry-run
```

### Pull Request Gates

Configure branch protection to require CI passing:
1. Repository Settings → Branches
2. Add rule for `master` branch
3. Require status checks: `build-suews-library`, `test-coupling-automation`

---

## Related Documentation

- **TESTING.md**: Manual testing procedures
- **QUICKSTART.md**: Local build instructions
- **GIT_WORKFLOW.md**: Branch and merge strategy
- **CLAUDE.md**: Project structure and workflows

---

## Troubleshooting Common CI Issues

### Issue: Submodule not initialized
```
ERROR: SUEWS/src/suews directory not found
```

**Solution**: Ensure `submodules: recursive` in checkout step

### Issue: NetCDF not found during SUEWS build
```
ERROR: Cannot find netcdf.mod
```

**Solution**: Add to dependencies:
```yaml
apt-get install -y libnetcdff-dev netcdf-bin
```

### Issue: Cache not restoring
```
ERROR: libsuews.a not found after cache restore
```

**Solution**: Check cache key matches between save/restore. Use `cache@v4` consistently.

### Issue: WRF configure hangs
```
ERROR: Workflow timeout after 6 hours
```

**Solution**: Use `printf` to provide input non-interactively:
```bash
printf "34\n1\n" | ./configure
```

---

**Last Updated**: 2025-11-21
**Workflow Version**: 1.0
**Maintainer**: See CLAUDE.md for contact information
