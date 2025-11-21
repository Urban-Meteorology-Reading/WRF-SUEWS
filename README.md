# WRF-SUEWS

WRF-SUEWS coupling project

## Current Versions (October 2025)

- **WRF**: v4.7.1 (updated from v4.0.2)
- **SUEWS**: 2025.10.15 from [UMEP-dev/SUEWS](https://github.com/UMEP-dev/SUEWS)
- **Note**: SUEWS repository has migrated from Urban-Meteorology-Reading to UMEP-dev organisation

## Getting Started

**After cloning the repository, initialise the submodules:**

``` bash
git submodule init
git submodule update
```
These commands fetch the WRF and SUEWS repositories that are coupled together.

## Guide for Compilation and Simulation

**Important Change (November 2025):** WRF-SUEWS now uses a library-based coupling approach. SUEWS is pre-compiled as a static library (`libsuews.a`) and linked with WRF, replacing the previous monolithic file merge approach. This improves build times by ~50%.

### [JASMIN](https://www.ceda.ac.uk/services/jasmin/) (as of November 2025)

Firstly make sure to use same compiler (preferably INTEL) for installing pre-requisite libraries and WRF model compilation.
To load intel compiler setting on JASMIN type `module load intel/20.0.0`

#### Dependencies
Please follow the official guide [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php) for other libraries requirement for the WRF compilation.

#### Setting WRF-SUEWS environment
Set the wrf-suews environment by typing `conda env create --file=wrf_suews.yml` and activate it by `conda activate wrf-suews` in the WRF-SUEWS directory.

#### Steps

**Option A: Automatic (Recommended)**
```bash
cd coupling-automator
make              # Automatically builds SUEWS library if needed, then sets up WRF coupling
cd ../compilation-YYYYMMDD
./configure       # Choose option 15 (Intel compiler), basic nesting
cd ../coupling-automator
make patch        # Patches configure.wrf for SUEWS library linking
cd ../compilation-YYYYMMDD
sbatch sb-compile.sh  # Submit compilation job
```

**Option B: Manual Control**

1. Build SUEWS library (first time only):
```bash
cd SUEWS/src/suews
make -f Makefile.lib PROFILE=ifort -j4
make -f Makefile.lib install
```

2. Set up WRF-SUEWS coupling:
```bash
cd ../../coupling-automator
make
```
This creates the `compilation-YYYYMMDD` folder (date-stamped compilation directory).

3. Configure WRF:
```bash
cd ../compilation-YYYYMMDD
./configure
```
Choose number `15` for Intel compiler and `basic` option for nesting.

4. Patch configure.wrf for SUEWS linking (automated):
```bash
cd ../coupling-automator
make patch
```
This automatically adds SUEWS library flags to `configure.wrf`.

5. Compile WRF-SUEWS:
```bash
cd ../compilation-YYYYMMDD
./compile em_real >& log.compile
# Or submit via SLURM:
sbatch sb-compile.sh
```


5. After compilation of the code, you need to transfer all the `wrfinput_d0*` files generated with WSPS to the location of main run (usually `./test/em_real` OR `./run`) (rename the files to the original names by removing .suews from the filenames). Also include the boundary condition `wrfbdy_d01` file in the run directory.

6. You also need to copy `namelist.suews` to the same location.

7. Use `LANDUSE.TBL` in `./test/em_real` to change the albedo associated with Urban areas (number `13` for `MODIFIED_IGBP_MODIS_NOAH` for both winter and summer. By default it is 15% (0.15). In London case, it is changed to 11% (0.11) based on Ward et al. 2016)

8. `namelist.input` should also be modified to be consistent for WRF-SUEWS. See examples [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/namelist_example/UK) (specially the `sf_surface_physics = 9` which specifies to use SUEWS as the LSM).

9. The rest of steps, are similar to usual WRF runs (running WRF-SUEWS)


### Apple Silicon (M-series chips) (as of November 2025)

**Current Versions:**
- WRF: v4.7.1
- SUEWS: 2025.10.15 (from UMEP-dev repository)
- **Build System**: Library-based coupling (pre-compiled `libsuews.a`)

#### Platform and Compiler

```
Darwin Kernel Version 24.6.0 (macOS 15.x)

GNU Fortran (Homebrew GCC 13.x or 14.x)
```

#### Prerequisites

1. Install Homebrew dependencies:
```bash
brew install gcc netcdf-c netcdf-fortran
```

2. Set environment variables for NetCDF:
```bash
export NETCDF=/opt/homebrew/
```
**Note:** This is set at the root of Homebrew to avoid "netcdf.inc" not found errors.

3. Optionally create symlink for GCC (if not automatically set):
```bash
# Check GCC version
which gcc-13 || which gcc-14

# Create symlink if needed
ln -s /opt/homebrew/bin/gcc-13 /usr/local/bin/gcc
# or for GCC 14
ln -s /opt/homebrew/bin/gcc-14 /usr/local/bin/gcc
```

#### Compilation Steps

**Option A: Automatic (Recommended)**
```bash
cd coupling-automator
make              # Builds SUEWS library + sets up WRF coupling
cd ../compilation-YYYYMMDD
./configure       # Choose option 15 (serial gcc/gfortran)
cd ../coupling-automator
make patch        # Auto-patches configure.wrf with SUEWS flags + GCC compatibility
cd ../compilation-YYYYMMDD
./compile em_real >& log.compile
```

**Option B: Manual Control**

1. Build SUEWS library:
```bash
cd SUEWS/src/suews
make -f Makefile.lib -j4
make -f Makefile.lib install
```

2. Set up WRF-SUEWS coupling:
```bash
cd ../../coupling-automator
make
```

3. Configure WRF:
```bash
cd ../compilation-YYYYMMDD
./configure
```
Choose option `15` for serial gcc/gfortran compiler

4. Patch configure.wrf (automated):
```bash
cd ../coupling-automator
make patch
```

**Manual alternative:** If you prefer to patch `configure.wrf` manually, add these flags:
```bash
FCBASEOPTS = $(FCBASEOPTS_NO_G) $(FCDEBUG) -fallow-invalid-boz -fallow-argument-mismatch
```

5. Compile WRF-SUEWS:
```bash
cd ../compilation-YYYYMMDD
./compile em_real >& log.compile
```

#### Important Notes for Apple Silicon

- **Library-Based Coupling**: SUEWS is now pre-compiled as `libsuews.a` rather than merged into a monolithic file
- **Build Time Improvement**: ~50% faster rebuilds (10-15 min → 5 min)
- **SUEWS Repository**: SUEWS has moved from `Urban-Meteorology-Reading` to `UMEP-dev` organisation
- **Source Location**: Changed from `SUEWS-SourceCode/` to `src/suews/`
- **Compiler Compatibility**: `-fallow-invalid-boz` and `-fallow-argument-mismatch` flags essential for GCC 10+
- **Automated Patching**: `make patch` handles all configure.wrf modifications automatically


## Pre-processing using WPS

To generate the original `wrfinput` files (before processing them for WRF-SUEWS), you should follow [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/CASES/JAN00/index.php). After generating `wrfinput` and `wrfbdy`, you need to follow pre-processing instructions to modify the input file suitbale for WRF-SUEWS runs


## SUEWS specific pre-processing using WRF-SUEWS preprocessing system (WSPS)
Please refer to this [instruction](./WSPS/README.md) for WRF-SUEWS preprocessing system (WSPS).

