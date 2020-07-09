# WRF-SUEWS

WRF-SUEWS coupling project

**Make sure you use the following commands after cloning the repo:**

``` bash
git submodule init
git submodule update
```
These commands update SUEWS repo associated with WRF-SUEWS. Currently, it is an older version of SUEWS than the most up-to-date one.

# To compile and Run:
1- Go to `coupling-automator`, and type `make`

2- It creates the WRF-SUEWS folder to compile (name of the folder depends on what you specify [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/50dba67f3a66cfee296d7c4de88d3f52353b13cd/coupling-automator/automate_main.py#L57))

3- In the created folder, type `./configure`. This is for configuration of WRF-SUEWS. Choose number `15` for the compiler and `basic` option for the nesting.

4- Then you need compile the code: `./compile em_real >& compile.log`. For this, you can submmit the job as following:

```bash
#!/bin/bash 
#BSUB -q short-serial 
#BSUB -o %J.out 
#BSUB -e %J.err 
#BSUB -W 02:30

./compile em_real >& log.compile

```
5- After compilation of the code, you need to transfer all the `wrf_input` files to the location of main run (usually `./test/em_real`). It should include the boundary condition file.

6- You also need to copy `namelist.suews` to the same location.

7- Use `LANDUSE.TBL` in `./test/em_real` to change the albedo associated with Urban aras (number `13` for `MODIFIED_IGBP_MODIS_NOAH` for both winter and summer. By default it is 15% (0.15). In London case, it is changed to 11%(0.11) based on Ward et al. 2016)

8- `namelist.input` should also be modified to be consistent for WRF-SUEWS. See examples.

9- The rest of steps, are similar to usual WRF runs (running WRF-SUEWS)

# WPS process

To generate the original `wrfinput` files (before processing them for WRF-SUEWS), you should follow [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/CASES/JAN00). After generating `wrfinput` and `wrfbdy`, you need to follow pre-processing instructions to modify the input file suitbale for WRF-SUEWS runs


# Preprocessing steps:

Here are typical preprocessing steps needed for `wrfinput` files to be ready for WRF-SUEWS runs:

Example: UK runs preprocessors from [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/2dcfb9bb5f208c3a0e39c1ad0d6bb3d283a88eee/input-processor/pre-processor-UK/WRF-SUEWS-preprocessor-UK.py#L11-L21)

```python
steps = {'clean_dirs': 1,
         'extract_params_cities': 1,
         'extract_params_vegs': 1,
         'extract_params_extra_lands': 1,
         'modify_trans': 1,
         'change_to_SUEWS': 1,
         'modify_London': 1,
         'parameters': 1,
         'timezone': 0
         }
```

`clean_dirs`: cleaning directories in [output](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output) folder before running the [main](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/master/input-processor/pre-processor-UK/WRF-SUEWS-preprocessor-UK.py) pre-processor code.

`extract_params_cities`: spining up SUEWS for cities  based on cities charactristics in the [runs folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/runs) - For all domains

`extract_params_vegs`: spining up SUEWS for pure vegetation grids (main land cover) - For all domains

`extract_params_extra_lands`: spining up SUEWS for other categories of land cover (combination of different land covers) - For all domains

`modify_trans`: modifying transmisivity - For all domains

`change_to_SUEWS`: modifying the variables in `wrfinput` files and adding SUEWS variables - For all domains

`modify_London`: modifying wrfinput variables related to greater London area (land fraction, building, vegetation height, QF coefficients etc.) - For most inner domain

`parameters`: modifying parameters related to non-urban areas such as albedo, LAI, conductances based on Omidvar et al. 2020 - For all domains

`timezone`: modifying grids timezone for all domains (might not work correctly because of the python package problem) - For all domains


**Each of the above steps is related to a utility function located [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/utility)**

**The input files (`wrf-inputs`) as well as non-urban parameters, and templates for `namelist.suews` and `SUEWS_param.json` are located in the [input folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/input)**

**Data related to land cover and building and vegetation height is located i [Data folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/data)**

## Environment to run pre-processors
Use conda to create a fresh environment for this pipeline using [environment.yml](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/master/input-processor/pre-processor-UK/environment.yml):

```bash
conda env create -f environment.yml
```
## Outputs
 Different versions of `wrfinput` files are saved in the [output folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output) during the run. The final output to be used for WRF-SUEWS runs will be written in the [final folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output/final)
