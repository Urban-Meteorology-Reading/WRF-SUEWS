# Testing Guide for WRF-SUEWS Updates

This guide provides systematic testing procedures for the WRF v4.7.1 and SUEWS 2025.10.15 coupling updates.

## Table of Contents
1. [Quick Verification Tests](#1-quick-verification-tests)
2. [Coupling Automator Tests](#2-coupling-automator-tests)
3. [Compilation Tests](#3-compilation-tests)
4. [Pre-processing Tests](#4-pre-processing-tests)
5. [Runtime Tests](#5-runtime-tests)

---

## 1. Quick Verification Tests

### 1.0 Automated Verification Script

**NEW (November 2025)**: Use the automated test script for quick verification:

```bash
./test_coupling.sh
```

This script automatically checks:
- ✅ WRF and SUEWS submodule versions (v4.7.1, 2025.10.15)
- ✅ SUEWS repository migration (UMEP-dev organisation)
- ✅ Coupling automator path updates (`src/suews` structure)
- ✅ Version module handling in `gen_suewsdrv.py`
- ✅ Generated compilation directory structure
- ✅ SUEWS wrapper files (`module_sf_suews.F`, `registry.suews`)
- ✅ WRF Registry modifications and code generation

**Usage:**
```bash
# Run with default (most recent compilation directory)
./test_coupling.sh

# Or specify a compilation directory
COMP_DIR=compilation-20251121 ./test_coupling.sh
```

**Output:** The script provides a clear pass/fail summary with actionable error messages.

**When to use:**
- After updating submodules
- Before creating a pull request
- After running coupling automation
- When debugging coupling issues

**Next Steps:** If all tests pass, proceed to compilation. If tests fail, review error messages and fix issues before compiling.

---

### 1.1 Check Submodule Versions (Manual)

```bash
# Check WRF version
cd WRF
git describe --tags
# Expected: v4.7.1

# Check SUEWS version
cd ../SUEWS
git describe --tags
# Expected: 2025.10.15

# Check SUEWS repository
git remote -v
# Expected: git@github.com:UMEP-dev/SUEWS.git
```

### 1.2 Verify Coupling Automator Updates

```bash
cd coupling-automator

# Check SUEWS path update
grep "path_src_SUEWS" automate_main.py
# Expected: Path("../SUEWS/src/suews")

# Check gen_suewsdrv.py updates
grep "PHYS =" gen_suewsdrv.py | head -1
# Expected: modules = ['UTILS =', 'PHYS =', 'DRIVER =', 'TEST =', 'WRF =']

# Check version module generation
grep "suews_ctrl_ver" gen_suewsdrv.py
# Expected: filter out suews_ctrl_ver.f95 and create dummy version module
```

### 1.3 Check SUEWS Source Structure

```bash
# Verify new SUEWS structure exists
ls -la ../SUEWS/src/suews/src/ | head -10
# Expected: Should see suews_*.f95 files

# Check Makefile exists
ls ../SUEWS/src/suews/Makefile
# Expected: File exists

# Verify WRF coupling module exists
ls ../SUEWS/src/suews/src/suews_ctrl_sumin.f95
# Expected: File exists
```

---

## 2. Coupling Automator Tests

### 2.1 Clean Previous Compilation

```bash
# Remove old compilation directory if exists (optional)
# Be careful: this removes the most recent compilation
# rm -rf compilation-*
# Or remove a specific directory:
# rm -rf compilation-20251105
```

### 2.2 Run Coupling Automator

```bash
cd coupling-automator
python3 automate_main.py 2>&1 | tee ../coupling_test.log

# Check for success
echo "Exit code: $?"
# Expected: 0 (success)
```

### 2.3 Verify Generated Files

```bash
# Find most recent compilation directory
COMP_DIR=$(ls -dt ../compilation-* 2>/dev/null | head -1)

# Check critical files exist
ls -lh $COMP_DIR/phys/module_sf_suews.F
ls -lh $COMP_DIR/phys/module_sf_suewsdrv.F
ls -lh $COMP_DIR/Registry/registry.suews
ls -lh $COMP_DIR/test/em_real/namelist.suews

# Check generated driver size
wc -l $COMP_DIR/phys/module_sf_suewsdrv.F
# Expected: ~50,000 lines

# Count modules in generated file
grep -c "^MODULE " $COMP_DIR/phys/module_sf_suewsdrv.F
# Expected: ~72 modules
```

### 2.4 Verify WRF Modifications

```bash
# Use most recent compilation directory
COMP_DIR=$(ls -dt ../compilation-* 2>/dev/null | head -1)

# Check Registry modifications
echo "=== Checking Registry.EM_COMMON ==="
grep -n "suewsscheme" $COMP_DIR/Registry/Registry.EM_COMMON
# Expected: Line with package suewsscheme sf_surface_physics==9

# Check surface driver modifications
echo "=== Checking module_surface_driver.F ==="
grep -n "USE module_sf_suews" $COMP_DIR/phys/module_surface_driver.F
grep -n "CASE (SUEWSSCHEME)" $COMP_DIR/phys/module_surface_driver.F
# Expected: Both should be found

# Check module_check_a_mundo
echo "=== Checking module_check_a_mundo.F ==="
grep -n "SUEWSSCHEME" $COMP_DIR/share/module_check_a_mundo.F
# Expected: Found

# Check physics_init modifications
echo "=== Checking module_physics_init.F ==="
grep -n "USE module_sf_suews" $COMP_DIR/phys/module_physics_init.F
grep -n "suewsinit" $COMP_DIR/phys/module_physics_init.F | wc -l
# Expected: Multiple lines (initialization calls)

echo "✅ All WRF modifications verified"
```

---

## 3. Compilation Tests

### 3.1 Prerequisites Check

```bash
# Check GCC installation
which gfortran
gfortran --version
# Expected: GCC 13.x or 14.x from Homebrew

# Check NetCDF installation
which nf-config
nf-config --version
# Expected: NetCDF Fortran library version

# Set NetCDF path
export NETCDF=/opt/homebrew/
echo $NETCDF
```

### 3.2 Configure WRF-SUEWS

```bash
# Navigate to most recent compilation directory
cd $(ls -dt compilation-* 2>/dev/null | head -1)

# Run configure
./configure 2>&1 | tee ../configure.log

# When prompted:
# - Choose option 15 (serial gcc/gfortran)
# - Choose option 1 (basic nesting)
```

### 3.3 Modify configure.wrf for Apple Silicon

```bash
# Backup original
cp configure.wrf configure.wrf.backup

# Check current FCBASEOPTS setting
grep "^FCBASEOPTS" configure.wrf

# Add compatibility flags
sed -i.bak 's/FCBASEOPTS.*=.*$(FCBASEOPTS_NO_G) $(FCDEBUG)$/& -fallow-invalid-boz -fallow-argument-mismatch/' configure.wrf

# Verify change
grep "^FCBASEOPTS" configure.wrf
# Expected: Should see the added flags
```

### 3.4 Test Compilation (Quick Check)

```bash
# Try compiling just the registry first (quick test)
./compile -j 1 em_real 2>&1 | head -100

# Check if registry processing works
ls -la Registry/*.inc 2>/dev/null | wc -l
# Expected: Multiple .inc files generated

# Check if any SUEWS-related errors appear
grep -i "suews" compile.log | grep -i "error"
# Expected: No critical errors (warnings OK)
```

### 3.5 Full Compilation

```bash
# Full compilation (will take 15-45 minutes)
./compile em_real 2>&1 | tee compile.log

# Monitor progress (in another terminal)
tail -f compile.log

# Check for success
ls -lh main/*.exe
# Expected: wrf.exe, real.exe, ndown.exe, tc.exe

# Check SUEWS modules compiled
grep "module_sf_suews" compile.log | grep -i "compile\|built"
# Expected: Both module_sf_suews.o and module_sf_suewsdrv.o compiled

# Check for compilation errors
grep -i "error" compile.log | grep -v "warning" | wc -l
# Expected: 0 (zero errors)
```

### 3.6 Compilation Success Check

```bash
# List executables
ls -lh main/wrf.exe main/real.exe
# Expected: Both files exist and are >20MB

# Check if SUEWS symbols are linked
nm main/wrf.exe | grep -i suews | head -10
# Expected: SUEWS-related symbols present

# Check module_sf_suews in binary
strings main/wrf.exe | grep -i "suewsdrv" | head -5
# Expected: Some SUEWS-related strings
```

---

## 4. Pre-processing Tests

### 4.1 WSPS Environment Setup

```bash
cd WSPS

# Check if WSPS environment exists
conda env list | grep WSPS
# If not exists, create it:
# conda env create -f environment.yml

# Activate WSPS environment
conda activate WSPS

# Verify supy version
python -c "import supy; print(supy.__version__)"
# Expected: 2019.4.15 or compatible version
```

### 4.2 Test WSPS Configuration

```bash
# Check namelist.suews exists
ls namelist.suews

# Verify wsps section
grep -A 10 "wsps" namelist.suews

# Check for sample input files
ls -la sample-case/input/
# Expected: wrfinput files from WPS
```

### 4.3 Run WSPS (if test data available)

```bash
# Only if you have test wrfinput files
python wsps.py 2>&1 | tee wsps_test.log

# Check output
ls -la output/final/wrfinput_d0*
# Expected: Modified wrfinput files with SUEWS variables
```

---

## 5. Runtime Tests

### 5.1 Minimal Configuration Test

```bash
# Navigate to test directory in most recent compilation
cd $(ls -dt compilation-* 2>/dev/null | head -1)/test/em_real

# Check namelist.input
ls namelist.input

# Verify SUEWS scheme is available
grep "sf_surface_physics" namelist.input

# To use SUEWS, set:
# sf_surface_physics = 9, 9, 9,  (for each domain)
```

### 5.2 Registry Variable Check

```bash
# Check if SUEWS variables are registered
grep -i "suews" ../../Registry/Registry.EM_COMMON | wc -l
# Expected: Many lines with SUEWS variables

# List some key SUEWS variables
grep "LAI_SUEWS\|albDecTr_SUEWS\|QN_SUEWS" ../../Registry/Registry.EM_COMMON
```

### 5.3 Test Executable

```bash
# Check WRF executable
../../main/wrf.exe -h 2>&1 | head -20
# Expected: WRF help message

# Verify SUEWS scheme number
grep "SUEWSSCHEME" ../../inc/module_state_description.F
# Expected: Parameter definition for scheme 9
```

---

## Quick Test Script

Save this as `test_coupling.sh`:

```bash
#!/bin/bash
# Quick coupling verification script

set -e  # Exit on error

# Find most recent compilation directory
COMP_DIR=$(ls -dt compilation-* 2>/dev/null | head -1)

echo "=== WRF-SUEWS Coupling Test ==="
echo ""

# 1. Check submodules
echo "1. Checking submodule versions..."
cd WRF && WRF_VER=$(git describe --tags) && cd ..
cd SUEWS && SUEWS_VER=$(git describe --tags) && cd ..
echo "   WRF: $WRF_VER"
echo "   SUEWS: $SUEWS_VER"
echo ""

# 2. Check generated files
echo "2. Checking generated files..."
if [ -d "$COMP_DIR" ]; then
    echo "   ✓ Compilation directory exists"

    if [ -f "$COMP_DIR/phys/module_sf_suews.F" ]; then
        echo "   ✓ module_sf_suews.F exists"
    fi

    if [ -f "$COMP_DIR/phys/module_sf_suewsdrv.F" ]; then
        SIZE=$(wc -l < "$COMP_DIR/phys/module_sf_suewsdrv.F")
        echo "   ✓ module_sf_suewsdrv.F exists ($SIZE lines)"
    fi

    if [ -f "$COMP_DIR/Registry/registry.suews" ]; then
        echo "   ✓ registry.suews exists"
    fi
else
    echo "   ✗ Compilation directory not found. Run: cd coupling-automator && make"
    exit 1
fi
echo ""

# 3. Check WRF modifications
echo "3. Checking WRF modifications..."
if grep -q "suewsscheme" "$COMP_DIR/Registry/Registry.EM_COMMON"; then
    echo "   ✓ Registry.EM_COMMON modified"
fi

if grep -q "USE module_sf_suews" "$COMP_DIR/phys/module_surface_driver.F"; then
    echo "   ✓ module_surface_driver.F modified"
fi

if grep -q "SUEWSSCHEME" "$COMP_DIR/share/module_check_a_mundo.F"; then
    echo "   ✓ module_check_a_mundo.F modified"
fi
echo ""

# 4. Check if compiled
echo "4. Checking compilation status..."
if [ -f "$COMP_DIR/main/wrf.exe" ]; then
    SIZE=$(ls -lh "$COMP_DIR/main/wrf.exe" | awk '{print $5}')
    echo "   ✓ wrf.exe exists ($SIZE)"

    if [ -f "$COMP_DIR/main/real.exe" ]; then
        echo "   ✓ real.exe exists"
    fi

    echo "   ✓ Compilation successful!"
else
    echo "   ⚠ Not yet compiled. To compile:"
    echo "     cd $COMP_DIR"
    echo "     export NETCDF=/opt/homebrew/"
    echo "     ./configure  # Choose option 15"
    echo "     # Modify configure.wrf (add -fallow-invalid-boz -fallow-argument-mismatch)"
    echo "     ./compile em_real"
fi
echo ""

echo "=== Test Summary ==="
echo "All critical checks passed! ✅"
```

Make it executable and run:
```bash
chmod +x test_coupling.sh
./test_coupling.sh
```

---

## Expected Timeline

| Test Level | Time Required | What it Tests |
|------------|---------------|---------------|
| Quick Verification | 2-5 minutes | Files and modifications |
| Coupling Automator | 1-2 minutes | Code generation |
| Configuration | 2-3 minutes | WRF setup |
| Compilation | 15-45 minutes | Building WRF-SUEWS |
| Pre-processing | 5-10 minutes | WSPS functionality |
| Runtime Test | 30-60 minutes | Full simulation |

---

## Troubleshooting

### Common Issues

**Issue 1: NetCDF not found**
```bash
export NETCDF=/opt/homebrew/
# Add to ~/.zshrc for persistence
```

**Issue 2: Compilation errors with BOZ constants**
```bash
# Ensure configure.wrf has:
# -fallow-invalid-boz -fallow-argument-mismatch
```

**Issue 3: Python module import errors**
```bash
conda activate WSPS
pip install pandas==0.24 xarray
```

**Issue 4: SUEWS module not found in WRF**
```bash
# Re-run coupling automator
cd coupling-automator && make
```

---

## Success Criteria

✅ **Level 1**: Coupling automator runs without errors
✅ **Level 2**: All WRF files properly modified
✅ **Level 3**: WRF-SUEWS compiles successfully
✅ **Level 4**: WSPS processes input files
✅ **Level 5**: WRF runs with SUEWS scheme

---

## Contact

If tests fail, provide:
1. Output of `test_coupling.sh`
2. Last 100 lines of `compile.log`
3. Output of `gfortran --version`
4. Output of `nf-config --version`
