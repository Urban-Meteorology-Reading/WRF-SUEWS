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

8- `namelist.input` should also be modified to be consistent for WRF-SUEWS. See examples [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/namelist_example/UK) (specially the `sf_surface_physics = 9` which specifies to use SUEWS as the LSM).

9- The rest of steps, are similar to usual WRF runs (running WRF-SUEWS)

# WPS process

To generate the original `wrfinput` files (before processing them for WRF-SUEWS), you should follow [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/CASES/JAN00/index.php). After generating `wrfinput` and `wrfbdy`, you need to follow pre-processing instructions to modify the input file suitbale for WRF-SUEWS runs


# WSPS steps:
Please refer to this [instruction](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/master/WSPS/README.md) for WRF-SUEWS preprocessing system (WSPS).

