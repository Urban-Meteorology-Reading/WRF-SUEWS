# Coupling Automator

This folder includes code to automate the workflow for WRF-SUEWS coupling.

## Python scripts

### `automate_main.py`

This is the main program to automate the workflow.

Three key variables needed by this program are:
1. `path_src_WRF`: path to WRF source code
2. `path_src_SUEWS`: path to SUEWS source code
3. `path_working`: path to a working directory for compiling the coupled system

To get the automation going, run:
```
python3 automate_main.py
```

Refer to the code for details.

### `gen_suewsdrv.py`

This script will be called by `automate_main.py` to generate Fortran code of the whole SUEWS for coupling.


## Files that need manual edits

### `changes_list.json`
a `json` file with dict-like structure to instruct modifications needed for WRF source code.

**TODO**: the multiline strings can be improved by changing them to list for better readability since the list can be easily merged at the python side.


### `module_sf_suews.F`

Fortran wrapper to include SUEWS code into WRF.


### `registry.suews`

A WRF `registry` file to instruct WRF for automated generation of data structures needed by SUEWS.

