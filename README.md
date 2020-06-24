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

3- In the created folder, type `./configure`. This is for configuration of WRF-SUEWS. Choose number `15` for the compiler and `basic` option in the next step.

4- Then you need compile the code: `./compile em_real >& compile.log`

5- After compilation of the code, you need to transfer all the `wrf_input` files to the location of main run (usually `./test/em_real`)

6- You also need to copy `namelist.suews` to the same location.

7- The rest of steps, are similar to usual WRF runs
