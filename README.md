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

### [JASMIN](https://www.ceda.ac.uk/services/jasmin/) (as of 04 May 2022)

Firstly make sure to use same compiler (preferably INTEL) for installing pre-requisite libraries and WRF model compilation.
To load intel compiler setting on JASMIN type `module load intel/20.0.0`

#### Dependencies
Please follow the official guide [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php) for other libraries requirement for the WRF compilation.

#### Setting WRF-SUEWS environment
Set the wrf-suews environment by typing `conda env create --file=wrf_suews.yml` and activate it by `conda activate wrf-suews` in the WRF-SUEWS directory.

#### Steps
1. Go to `coupling-automator` folder, and type `make`

2. It creates the `compilation-YYYYMMDD` folder to compile (name of the folder depends on what you specify [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/50dba67f3a66cfee296d7c4de88d3f52353b13cd/coupling-automator/automate_main.py#L57))

3. In the created folder, type `./configure`
This is for configuration of WRF-SUEWS. Choose number `15` for the compiler (as of WRFv4 this refers to standard intel compiler) and `basic` option for the nesting.

4. Then you need compile the code: `./compile em_real >& log.compile`. For this, you can submit the [job file](./jasmin-config/sb-compile.sh) by `sbatch sb-compile.sh` in the compilation folder (specified by `path_working` in [automate_main.py](./coupling-automator/automate_main.py)).


5. After compilation of the code, you need to transfer all the `wrfinput_d0*` files generated with WSPS to the location of main run (usually `./test/em_real` OR `./run`) (rename the files to the original names by removing .suews from the filenames). Also include the boundary condition `wrfbdy_d01` file in the run directory.

6. You also need to copy `namelist.suews` to the same location.

7. Use `LANDUSE.TBL` in `./test/em_real` to change the albedo associated with Urban areas (number `13` for `MODIFIED_IGBP_MODIS_NOAH` for both winter and summer. By default it is 15% (0.15). In London case, it is changed to 11% (0.11) based on Ward et al. 2016)

8. `namelist.input` should also be modified to be consistent for WRF-SUEWS. See examples [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/namelist_example/UK) (specially the `sf_surface_physics = 9` which specifies to use SUEWS as the LSM).

9. The rest of steps, are similar to usual WRF runs (running WRF-SUEWS)


### Apple Silicon (M-series chips) (as of 26 Oct 2025)

**Current Versions:**
- WRF: v4.7.1
- SUEWS: 2025.10.15 (from UMEP-dev repository)

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

1. Generate compilation working directory:
```bash
cd coupling-automator
make
```

2. Configure WRF-SUEWS:
```bash
cd compilation-YYYYMMDD  # Date will be today's date (e.g., compilation-20251026)
./configure
```
Choose option `15` for serial gcc/gfortran compiler

3. Modify `configure.wrf` for Apple Silicon compatibility:

Find the following line in `configure.wrf`:
```bash
FCBASEOPTS      =       $(FCBASEOPTS_NO_G) $(FCDEBUG)
```

Add compatibility flags:
```bash
FCBASEOPTS      =       $(FCBASEOPTS_NO_G) $(FCDEBUG) -fallow-invalid-boz -fallow-argument-mismatch
```

4. Compile WRF-SUEWS:
```bash
./compile em_real >& log.compile
```

#### Important Notes for Apple Silicon

- **SUEWS Repository Change**: SUEWS has moved from `Urban-Meteorology-Reading` to `UMEP-dev` organisation on GitHub
- **New SUEWS Structure**: Source code location changed from `SUEWS-SourceCode` to `src/suews`
- **Version Module**: Version information is now auto-generated for WRF coupling
- **Compiler Flags**: The `-fallow-invalid-boz` and `-fallow-argument-mismatch` flags are essential for GCC 10+ compatibility


## Pre-processing using WPS

To generate the original `wrfinput` files (before processing them for WRF-SUEWS), you should follow [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/CASES/JAN00/index.php). After generating `wrfinput` and `wrfbdy`, you need to follow pre-processing instructions to modify the input file suitbale for WRF-SUEWS runs


## SUEWS specific pre-processing using WRF-SUEWS preprocessing system (WSPS)
Please refer to this [instruction](./WSPS/README.md) for WRF-SUEWS preprocessing system (WSPS).

