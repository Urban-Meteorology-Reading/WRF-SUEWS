#!/bin/bash
# Quick coupling verification script for WRF-SUEWS updates

set -e  # Exit on error

COMP_DIR="compilation-$(date +%Y%m%d)"
ERRORS=0

echo "=========================================="
echo "  WRF-SUEWS Coupling Verification Test"
echo "=========================================="
echo ""

# 1. Check submodules
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "1. Checking Submodule Versions"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
cd WRF
WRF_VER=$(git describe --tags 2>/dev/null || echo "unknown")
if [[ "$WRF_VER" == v4.7.1* ]]; then
    echo "   ✅ WRF: $WRF_VER (v4.7.1)"
else
    echo "   ❌ WRF: $WRF_VER (expected v4.7.1)"
    ERRORS=$((ERRORS + 1))
fi
cd ..

cd SUEWS
SUEWS_VER=$(git describe --tags 2>/dev/null || echo "unknown")
SUEWS_REMOTE=$(git remote get-url origin 2>/dev/null || echo "unknown")
if [[ "$SUEWS_VER" == 2025.10.15* ]]; then
    echo "   ✅ SUEWS: $SUEWS_VER (2025.10.15)"
else
    echo "   ❌ SUEWS: $SUEWS_VER (expected 2025.10.15)"
    ERRORS=$((ERRORS + 1))
fi

if [[ "$SUEWS_REMOTE" == *"UMEP-dev"* ]]; then
    echo "   ✅ Repository: UMEP-dev/SUEWS"
else
    echo "   ❌ Repository: $SUEWS_REMOTE (expected UMEP-dev)"
    ERRORS=$((ERRORS + 1))
fi
cd ..
echo ""

# 2. Check coupling automator files
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "2. Checking Coupling Automator Updates"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if grep -q 'Path("../SUEWS/src/suews")' coupling-automator/automate_main.py; then
    echo "   ✅ automate_main.py: SUEWS path updated"
else
    echo "   ❌ automate_main.py: SUEWS path not updated"
    ERRORS=$((ERRORS + 1))
fi

if grep -q "suews_ctrl_ver.f95" coupling-automator/gen_suewsdrv.py; then
    echo "   ✅ gen_suewsdrv.py: Version module handling added"
else
    echo "   ❌ gen_suewsdrv.py: Version module handling missing"
    ERRORS=$((ERRORS + 1))
fi

if [ -f "SUEWS/src/suews/Makefile" ]; then
    echo "   ✅ SUEWS structure: New src/suews path exists"
else
    echo "   ❌ SUEWS structure: New path missing"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# 3. Check compilation directory
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "3. Checking Generated Compilation Directory"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ -d "$COMP_DIR" ]; then
    echo "   ✅ Directory: $COMP_DIR exists"

    # Check critical files
    if [ -f "$COMP_DIR/phys/module_sf_suews.F" ]; then
        LINES=$(wc -l < "$COMP_DIR/phys/module_sf_suews.F")
        echo "   ✅ module_sf_suews.F: exists ($LINES lines)"
    else
        echo "   ❌ module_sf_suews.F: missing"
        ERRORS=$((ERRORS + 1))
    fi

    if [ -f "$COMP_DIR/phys/module_sf_suewsdrv.F" ]; then
        SIZE=$(wc -l < "$COMP_DIR/phys/module_sf_suewsdrv.F")
        MODULES=$(grep -c "^MODULE " "$COMP_DIR/phys/module_sf_suewsdrv.F" 2>/dev/null || echo 0)
        if [ "$SIZE" -gt 40000 ]; then
            echo "   ✅ module_sf_suewsdrv.F: $SIZE lines, $MODULES modules"
        else
            echo "   ⚠️  module_sf_suewsdrv.F: $SIZE lines (expected ~50k)"
        fi
    else
        echo "   ❌ module_sf_suewsdrv.F: missing"
        ERRORS=$((ERRORS + 1))
    fi

    if [ -f "$COMP_DIR/Registry/registry.suews" ]; then
        echo "   ✅ registry.suews: exists"
    else
        echo "   ❌ registry.suews: missing"
        ERRORS=$((ERRORS + 1))
    fi

    if [ -f "$COMP_DIR/test/em_real/namelist.suews" ]; then
        echo "   ✅ namelist.suews: exists"
    else
        echo "   ❌ namelist.suews: missing"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "   ❌ Compilation directory not found!"
    echo "   Run: cd coupling-automator && make"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# 4. Check WRF modifications (only if compilation dir exists)
if [ -d "$COMP_DIR" ]; then
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "4. Verifying WRF File Modifications"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    # Check Registry.EM_COMMON
    if grep -q "package   suewsscheme" "$COMP_DIR/Registry/Registry.EM_COMMON"; then
        NVARS=$(grep "package   suewsscheme" "$COMP_DIR/Registry/Registry.EM_COMMON" | grep -o "SUEWS" | wc -l)
        echo "   ✅ Registry.EM_COMMON: SUEWS scheme registered (~$NVARS variables)"
    else
        echo "   ❌ Registry.EM_COMMON: SUEWS scheme not found"
        ERRORS=$((ERRORS + 1))
    fi

    # Check Registry.EM
    if grep -q "include registry.suews" "$COMP_DIR/Registry/Registry.EM"; then
        echo "   ✅ Registry.EM: includes registry.suews"
    else
        echo "   ❌ Registry.EM: missing registry.suews include"
        ERRORS=$((ERRORS + 1))
    fi

    # Check module_surface_driver.F
    if grep -q "USE module_sf_suews" "$COMP_DIR/phys/module_surface_driver.F"; then
        if grep -q "CASE (SUEWSSCHEME)" "$COMP_DIR/phys/module_surface_driver.F"; then
            echo "   ✅ module_surface_driver.F: SUEWS integration complete"
        else
            echo "   ⚠️  module_surface_driver.F: USE statement found but CASE missing"
        fi
    else
        echo "   ❌ module_surface_driver.F: SUEWS not integrated"
        ERRORS=$((ERRORS + 1))
    fi

    # Check module_physics_init.F
    if grep -q "USE module_sf_suews" "$COMP_DIR/phys/module_physics_init.F"; then
        NCALLS=$(grep -c "suewsinit" "$COMP_DIR/phys/module_physics_init.F" 2>/dev/null || echo 0)
        echo "   ✅ module_physics_init.F: SUEWS init added ($NCALLS references)"
    else
        echo "   ❌ module_physics_init.F: SUEWS init missing"
        ERRORS=$((ERRORS + 1))
    fi

    # Check module_check_a_mundo.F
    if grep -q "SUEWSSCHEME" "$COMP_DIR/share/module_check_a_mundo.F"; then
        echo "   ✅ module_check_a_mundo.F: SUEWS scheme check added"
    else
        echo "   ❌ module_check_a_mundo.F: SUEWS check missing"
        ERRORS=$((ERRORS + 1))
    fi

    # Check module_first_rk_step_part1.F
    if grep -q "LAI_SUEWS" "$COMP_DIR/dyn_em/module_first_rk_step_part1.F"; then
        echo "   ✅ module_first_rk_step_part1.F: SUEWS variables passed"
    else
        echo "   ❌ module_first_rk_step_part1.F: SUEWS variables not passed"
        ERRORS=$((ERRORS + 1))
    fi

    # Check Makefile
    if grep -q "module_sf_suews.o" "$COMP_DIR/phys/Makefile"; then
        echo "   ✅ phys/Makefile: SUEWS modules added"
    else
        echo "   ❌ phys/Makefile: SUEWS modules missing"
        ERRORS=$((ERRORS + 1))
    fi
    echo ""
fi

# 5. Check compilation status
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "5. Checking Compilation Status"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ -f "$COMP_DIR/main/wrf.exe" ]; then
    SIZE=$(ls -lh "$COMP_DIR/main/wrf.exe" | awk '{print $5}')
    echo "   ✅ wrf.exe exists ($SIZE)"

    if [ -f "$COMP_DIR/main/real.exe" ]; then
        echo "   ✅ real.exe exists"
    fi

    # Check if SUEWS symbols are in the binary
    if nm "$COMP_DIR/main/wrf.exe" 2>/dev/null | grep -qi "suews"; then
        echo "   ✅ SUEWS symbols found in wrf.exe"
    else
        echo "   ⚠️  SUEWS symbols not found (may be OK)"
    fi

    echo ""
    echo "   🎉 COMPILATION SUCCESSFUL!"
else
    echo "   ⚠️  Not yet compiled"
    echo ""
    echo "   To compile:"
    echo "   ─────────────────────────────────────────"
    echo "   cd $COMP_DIR"
    echo "   export NETCDF=/opt/homebrew/"
    echo "   ./configure  # Choose option 15"
    echo "   # Edit configure.wrf: add -fallow-invalid-boz -fallow-argument-mismatch"
    echo "   ./compile em_real >& log.compile"
    echo ""
fi
echo ""

# 6. Environment check
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "6. Checking Build Environment"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check gfortran
if command -v gfortran &> /dev/null; then
    GCC_VER=$(gfortran --version | head -1)
    echo "   ✅ gfortran: $GCC_VER"
else
    echo "   ❌ gfortran: not found"
    ERRORS=$((ERRORS + 1))
fi

# Check NetCDF
if command -v nf-config &> /dev/null; then
    NC_VER=$(nf-config --version)
    echo "   ✅ NetCDF: $NC_VER"
else
    echo "   ❌ nf-config: not found"
    echo "      Install: brew install netcdf-c netcdf-fortran"
    ERRORS=$((ERRORS + 1))
fi

# Check NETCDF environment variable
if [ -n "$NETCDF" ]; then
    echo "   ✅ NETCDF environment: $NETCDF"
else
    echo "   ⚠️  NETCDF environment: not set"
    echo "      Run: export NETCDF=/opt/homebrew/"
fi
echo ""

# Summary
echo "=========================================="
echo "  Test Summary"
echo "=========================================="
if [ $ERRORS -eq 0 ]; then
    echo "✅ All checks passed!"
    echo ""
    echo "Status: Ready for compilation"
    echo ""
    echo "Next steps:"
    echo "  1. cd $COMP_DIR"
    echo "  2. export NETCDF=/opt/homebrew/"
    echo "  3. ./configure (choose 15)"
    echo "  4. Modify configure.wrf"
    echo "  5. ./compile em_real"
else
    echo "❌ Found $ERRORS error(s)"
    echo ""
    echo "Please review errors above and:"
    echo "  - Ensure submodules are properly updated"
    echo "  - Re-run: cd coupling-automator && make"
    echo "  - Check TESTING.md for detailed troubleshooting"
fi
echo "=========================================="
