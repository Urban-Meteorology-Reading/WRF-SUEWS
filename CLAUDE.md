# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

WRF-SUEWS is a coupled atmospheric-urban surface model combining:
- **WRF** (Weather Research and Forecasting) model v4.7.1
- **SUEWS** (Surface Urban Energy and Water Balance Scheme) 2025.10.15

Both WRF and SUEWS are included as **git submodules** - never modify them directly.

## Repository Structure

```
.
├── WRF/                      # WRF submodule (do NOT modify)
├── SUEWS/                    # SUEWS submodule (do NOT modify)
├── coupling-automator/       # Core coupling automation scripts
├── WSPS/                     # WRF-SUEWS Pre-processor System
├── compilation-YYYYMMDD/     # Generated compilation directory
├── jasmin-config/           # JASMIN HPC configuration
└── docs/                    # Documentation
```

## Key Workflows

### 1. Initial Setup

After cloning, initialise submodules:
```bash
git submodule init
git submodule update
```

### 2. Building SUEWS Library (November 2025: Library-Based Approach)

**New workflow**: SUEWS is now pre-compiled as a static library instead of being merged into WRF source.

**Option A: Automatic (Recommended)**
```bash
cd coupling-automator
make              # Automatically builds SUEWS library if missing
```

**Option B: Manual**
```bash
cd SUEWS/src/suews
make -f Makefile.lib -j4
make -f Makefile.lib install
cd ../../coupling-automator
make
```

The library build (`Makefile.lib`):
1. Compiles SUEWS source files in dependency order
2. Includes SPARTACUS radiation modules
3. Creates `SUEWS/lib/libsuews.a` (static library)
4. Installs Fortran modules to `SUEWS/include/*.mod`

### 3. Set Up WRF-SUEWS Coupling

```bash
cd coupling-automator
make
```

This runs `automate_main.py` which:
1. Verifies SUEWS library exists (exits if missing)
2. Creates `compilation-YYYYMMDD/` directory (today's date)
3. Copies WRF source to the compilation directory
4. Applies coupling modifications from `changes_list.json`
5. Copies wrapper files (`module_sf_suews.F`, `registry.suews`, `namelist.suews`)
6. **No longer generates `module_sf_suewsdrv.F`** (uses library instead)

**Important**: The script removes existing `compilation-YYYYMMDD/` if present.

### 4. Configure and Patch

After coupling setup completes:

```bash
cd compilation-YYYYMMDD/
./configure
```

**Platform-specific configuration:**
- **JASMIN**: Option 15 (Intel compiler)
- **Apple Silicon (M-series)**: Option 15 (serial gcc/gfortran)

**Patch configure.wrf (automated):**
```bash
cd ../coupling-automator
make patch
```

This automatically:
- Includes `wrf_suews.mk` configuration
- Adds SUEWS library paths to linker flags
- Adds SUEWS module paths to compiler flags
- **(Apple Silicon)** Adds GCC compatibility flags

**Manual alternative:** Edit `configure.wrf` to add:
```bash
include $(WRF_SRC_ROOT_DIR)/../SUEWS/lib/wrf_suews.mk
LIB = ... $(SUEWS_LDFLAGS)
INCLUDE_MODULES = ... $(SUEWS_CPPFLAGS)
# For Apple Silicon, also add:
FCBASEOPTS = $(FCBASEOPTS_NO_G) $(FCDEBUG) -fallow-invalid-boz -fallow-argument-mismatch
```

### 5. Compile WRF-SUEWS

```bash
cd compilation-YYYYMMDD/
./compile em_real >& log.compile
```

On JASMIN, submit via SLURM: `sbatch sb-compile.sh`

### 6. Pre-processing with WSPS

The WRF-SUEWS Pre-processor System (WSPS) modifies `wrfinput_d0*.nc` files from WPS to include SUEWS-specific variables.

**Setup:**
```bash
cd WSPS
conda env create -f environment.yml
conda activate WSPS
```

**Configuration:**
Edit `wsps` section in `namelist.suews` to specify:
- Urban sites for spin-up (`urban_site_spin_up`)
- Domain numbers (`urban_domain_number`)
- Urban class thresholds (`urban_class_threshold`)
- Vegetation site (`veg_site_spin_up`)
- Start date and file paths

Add SUEWS site configuration files to `sample-case/input/spin_ups/` and WPS-generated `wrfinput_d0*.nc` files to `sample-case/input/`.

**Run:**
```bash
python wsps.py
```

Modified files appear in `output/final/`.

**Optional site-specific modifications:**
```bash
python wsps_site_specific.py
```

Uses custom modules in `utility/site_specific/` (see London/Swindon examples).

### 7. Running WRF-SUEWS

Copy modified inputs to run directory:
```bash
cp output/final/wrfinput_d0* compilation-YYYYMMDD/test/em_real/
cp WSPS/namelist.suews compilation-YYYYMMDD/test/em_real/
```

Ensure `namelist.input` sets `sf_surface_physics = 9` (SUEWS as LSM).

Run as normal WRF simulation.

## Architecture

### Coupling Mechanism (Library-Based - November 2025)

The coupling is implemented through automated source code modification and static library linking:

1. **Registry System** (`registry.suews`): Defines SUEWS-specific state variables and metadata that WRF's automated code generation system uses to create data structures.

2. **WRF Modifications** (`changes_list.json`): JSON file specifying insertions into WRF source files:
   - `Registry.EM_COMMON`: Adds SUEWS package and variables
   - `module_physics_init.F`: Initialises SUEWS interface
   - `module_radiation_driver.F`: Integrates SUEWS into radiation calculations
   - `start_em.F`: Passes SUEWS variables during initialisation

3. **SUEWS Library** (`libsuews.a`): Pre-compiled static library containing all SUEWS source code:
   - Built separately from WRF using `Makefile.lib`
   - Includes SPARTACUS radiation modules
   - Location: `SUEWS/lib/libsuews.a`
   - Module files: `SUEWS/include/*.mod`

4. **SUEWS Wrapper** (`module_sf_suews.F`): Fortran interface layer that calls SUEWS from WRF's surface physics framework.
   - Links against `libsuews.a` at compile time
   - Thin wrapper - contains no SUEWS implementation code

**Key Difference from Previous Approach:**
- **Old**: Generated 61,000-line `module_sf_suewsdrv.F` by merging all SUEWS files
- **New**: Links against pre-compiled `libsuews.a` library
- **Benefit**: 50% faster rebuilds, incremental SUEWS updates

### Key Python Scripts

**`automate_main.py`**:
- Main automation script
- Defines paths: `path_src_WRF`, `path_src_SUEWS`, `path_working`
- Orchestrates the entire coupling workflow
- New functions (library-based):
  - `check_suews_library()`: Verifies library exists before coupling
  - `inject_suews_link_flags()`: Auto-patches configure.wrf for SUEWS linking

**`gen_suewsdrv.py`** (deprecated):
- **No longer used** with library-based approach
- Previously merged SUEWS source files into monolithic `module_sf_suewsdrv.F`
- Kept for reference only

### SUEWS Structure Changes (2025)

Recent SUEWS repository migration:
- **Organisation**: Urban-Meteorology-Reading → UMEP-dev
- **Source location**: `SUEWS-SourceCode/` → `src/suews/`
- **Version handling**: Auto-generated for WRF coupling

## Environment Setup

### Main Environment (wrf-suews.yml)
```bash
conda env create -f wrf-suews.yml
conda activate wrf-suews
```

Includes: Python 3.9, pandas, xarray, supy, wrf-python, geopandas, etc.

### WSPS Environment (WSPS/environment.yml)
```bash
conda env create -f WSPS/environment.yml
conda activate WSPS
```

Includes: Python 3.7, supy 2019.4.15, pandas 0.24, xarray

## Platform-Specific Notes

### JASMIN HPC
- Use Intel compiler: `module load intel/20.0.0`
- Configuration option: 15
- Submit compilation via SLURM

### Apple Silicon (M-series)
- Requires Homebrew GCC 13/14
- NetCDF via Homebrew: `brew install gcc netcdf-c netcdf-fortran`
- Compiler flags required for GCC 10+ compatibility
- Configuration option: 15 (serial)

## Common Development Tasks

### Modifying Coupling Behaviour

1. **Add WRF variables**: Edit `registry.suews`
2. **Change WRF modifications**: Edit `changes_list.json`
3. **Update SUEWS wrapper**: Edit `module_sf_suews.F`
4. **Rebuild**: `cd coupling-automator && make`

### Updating SUEWS Version

```bash
cd SUEWS
git fetch
git checkout <new-version-tag>
cd ..
git add SUEWS
git commit -m "Update SUEWS to <version>"
```

Then rebuild coupling.

### Updating WRF Version

```bash
cd WRF
git fetch
git checkout <new-version-tag>
cd ..
git add WRF
git commit -m "Update WRF to <version>"
```

Review `changes_list.json` for compatibility with new WRF structure.

## Important Files

- `changes_list.json`: Defines all WRF source modifications
- `registry.suews`: WRF registry file for SUEWS variables
- `module_sf_suews.F`: SUEWS wrapper for WRF
- `namelist.suews`: SUEWS runtime configuration
- `configure.wrf`: Generated by WRF configure script (modify for Apple Silicon)

## Testing

No automated test suite exists. Manual testing workflow:
1. Build successfully on target platform
2. Run WSPS on sample case
3. Execute short WRF-SUEWS simulation
4. Verify output files and energy balance closure

## Known Issues

- Symlinks in WRF may not copy correctly during automation (non-critical)
- GCC 10+ requires compatibility flags for legacy Fortran (BOZ, argument mismatches)
- WSPS requires older Python (3.7) and specific supy version (2019.4.15)

## Documentation

- Main README: Project overview and compilation guide
- `coupling-automator/README.md`: Automation details
- `WSPS/README.md`: Pre-processor configuration and usage
- SUEWS docs: [UMEP-dev/SUEWS](https://github.com/UMEP-dev/SUEWS)
- WRF docs: [WRF Online Tutorial](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/)
