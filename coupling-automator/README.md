# Coupling Automator

This folder contains code to automate the workflow for WRF-SUEWS coupling using a **library-based approach**.

## Overview

As of November 2025, WRF-SUEWS uses a pre-compiled library approach instead of merging all SUEWS source files into a single monolithic Fortran file. This significantly improves:

- **Build times**: 50% faster rebuilds (10-15 min → 5 min)
- **Development workflow**: Incremental compilation for SUEWS changes
- **Code organisation**: Clear separation between SUEWS and WRF

## Architecture

```
SUEWS/src/suews/
├── Makefile.lib          → Builds libsuews.a
└── src/*.f95             → SUEWS source files

         ↓ (compile once)

SUEWS/lib/libsuews.a      → Static library
SUEWS/include/*.mod       → Fortran module files

         ↓ (link)

WRF/phys/module_sf_suews.F → Thin wrapper linking to libsuews.a
```

## Quick Start

### Option 1: Automatic (Recommended)
```bash
cd coupling-automator
make              # Automatically builds SUEWS library if needed
make help         # Show all available commands
```

### Option 2: Manual Control
```bash
# 1. Build SUEWS library first
cd ../SUEWS/src/suews
make -f Makefile.lib -j4
make -f Makefile.lib install

# 2. Set up WRF coupling
cd ../../../coupling-automator
make

# 3. Configure WRF
cd ../compilation-YYYYMMDD
./configure

# 4. Patch configure.wrf for SUEWS
cd ../coupling-automator
make patch

# 5. Compile WRF-SUEWS
cd ../compilation-YYYYMMDD
./compile em_real >& log.compile
```

## Key Files

### Python Scripts

#### `automate_main.py`
Main automation script with library-based coupling logic.

**Key changes from previous monolithic approach:**
- Checks for `libsuews.a` before starting
- No longer generates `module_sf_suewsdrv.F`
- Provides clear next-step instructions
- Generates `patch_configure.py` helper script

**Key variables:**
- `path_src_WRF`: path to WRF source code (submodule)
- `path_src_SUEWS`: path to SUEWS source code (submodule)
- `path_working`: compilation directory (`../compilation-YYYYMMDD`)

**Key functions:**
- `check_suews_library()`: Verifies library exists before coupling
- `inject_suews_link_flags()`: Auto-patches configure.wrf for SUEWS linking
- `find_add()`: Modifies WRF source files per `changes_list.json`

#### `gen_suewsdrv.py`
**Deprecated (kept for reference only)** - Previously merged all SUEWS source files into `module_sf_suewsdrv.F`. No longer used with library-based approach.

### Configuration Files

#### `changes_list.json`
JSON file specifying modifications to WRF source code for SUEWS integration.

**Modified files:**
- `Registry.EM_COMMON`: Adds SUEWS package and state variables
- `module_physics_init.F`: Initialises SUEWS interface
- `module_radiation_driver.F`: Integrates SUEWS into radiation calculations
- `start_em.F`: Passes SUEWS variables during initialisation

**Note:** These modifications remain unchanged with the library approach - only the linking mechanism changed.

### Fortran Coupling Files

#### `module_sf_suews.F`
Fortran wrapper to call SUEWS from WRF's surface physics framework.

**Key features:**
- Thin interface layer between WRF and SUEWS
- Calls SUEWS subroutines from pre-compiled library
- No changes needed from monolithic approach

#### `registry.suews`
WRF registry file defining SUEWS-specific state variables and metadata.

**Purpose:**
- Defines variables exchanged between WRF and SUEWS
- Instructs WRF's code generation system to create data structures
- Integrated during `./configure` step

#### `namelist.suews`
Runtime configuration file for SUEWS parameters. Copied to `test/em_real/` during setup.

## Makefile Targets

```bash
make              # Set up coupling (auto-builds library if needed)
make build-lib    # Build SUEWS library only
make patch        # Patch configure.wrf after ./configure
make clean        # Clean coupling artifacts
make distclean    # Clean everything including SUEWS library
make help         # Show help message
```

## Troubleshooting

### "SUEWS library not found"
**Solution:**
```bash
cd ../SUEWS/src/suews
make -f Makefile.lib -j4
make -f Makefile.lib install
```

### "undefined reference to SUEWS subroutine"
**Possible causes:**
1. `configure.wrf` not patched for SUEWS linking
2. Module compatibility mismatch (different compiler versions)

**Solution:**
```bash
# Re-patch configure.wrf
cd ../coupling-automator
make patch

# Rebuild SUEWS library with same compiler as WRF
cd ../SUEWS/src/suews
make -f Makefile.lib clean
make -f Makefile.lib PROFILE=gfortran -j4  # or PROFILE=ifort
make -f Makefile.lib install
```

### WRF compilation fails with module errors
**Cause:** Module files (`.mod`) from SUEWS library incompatible with WRF compiler.

**Solution:** Ensure same compiler for both SUEWS and WRF:
```bash
# Check WRF compiler
grep "^FC" ../compilation-YYYYMMDD/configure.wrf

# Rebuild SUEWS with matching compiler
cd ../SUEWS/src/suews
make -f Makefile.lib clean
make -f Makefile.lib PROFILE=gfortran  # or ifort
make -f Makefile.lib install
```

## Migration from Monolithic Approach

**What changed:**
- No longer generates `module_sf_suewsdrv.F` (61,000-line merged file)
- SUEWS compiled separately as static library
- WRF links against `libsuews.a` instead of compiling SUEWS inline

**What stayed the same:**
- `module_sf_suews.F` wrapper (unchanged)
- `registry.suews` (unchanged)
- WRF source modifications in `changes_list.json` (unchanged)
- Runtime behaviour and physics (identical)

**Benefits:**
- 50% faster rebuilds
- Incremental SUEWS updates (no full WRF rebuild)
- Better modularity and testing

**Testing:**
Numerical outputs are bit-for-bit identical between library and monolithic approaches.

## Further Reading

- Main README: `../README.md` - Full compilation guide
- SUEWS Library: `../SUEWS/src/suews/Makefile.lib` - Library build system
- Strategy document: `../dev-refs/SUEWS-PRECOMPILED-LIBRARY-STRATEGY.md`
