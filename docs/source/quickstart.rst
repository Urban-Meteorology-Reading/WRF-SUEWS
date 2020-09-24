.. _quickstart:

Quick Start
============



Installation
-----------------

Download source code
~~~~~~~~~~~~~~~~~~~~

Clone WRF-SUEWS repository::

    git clone git@github.com:Urban-Meteorology-Reading/WRF-SUEWS.git


After cloning the repo, make sure you use the following commands to update SUEWS repo associated with WRF-SUEWS ::

    git submodule init
    git submodule update


.. note:: Currently, it is an older version of SUEWS than the most up-to-date one.


Couple WRF and SUEWS
~~~~~~~~~~~~~~~~~~~~~

Suppose you are in the root location of ``WRF-SUEWS`` directory, run the following code to set up a workspace folder for the coupled code (a folder named ``WRF-SUEWS`` by default; `the name can be customised <https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/50dba67f3a66cfee296d7c4de88d3f52353b13cd/coupling-automator/automate_main.py#L57>`)::

    # go to the coupler directory
    cd coupling-automator

    # run the coupler
    make

Compile WRF-SUEWS
~~~~~~~~~~~~~~~~~~~~~

Then go into the workspace folder (``WRF-SUEWS`` if not set otherwise) and configure the compilation, which is now same as the standard WRF workflow::

    ./configure

Compile the code as follows::

    ./compile em_real >& compile.log


.. note:: If working on `jasmin`, you can submit the job as following:


    .. unclear: what is this file?

    .. code-block:: bash

        #!/bin/bash
        #BSUB -q short-serial
        #BSUB -o %J.out
        #BSUB -e %J.err
        #BSUB -W 02:30

        ./compile em_real >& log.compile

.. what is this? is this preprocessing?

5- After compilation of the code, you need to transfer all the `wrf_input` files to the location of main run (usually `./test/em_real`). It should include the boundary condition file (this step should be done after pre-processing steps).

6- You also need to copy `namelist.suews` to the same location (this step should be done after pre-processing steps).

7- Use `LANDUSE.TBL` in `./test/em_real` to change the albedo associated with Urban areas (number `13` for `MODIFIED_IGBP_MODIS_NOAH` for both winter and summer. By default it is 15% (0.15). In London case, it is changed to 11%(0.11) based on Ward et al. 2016)

8- `namelist.input` should also be modified to be consistent for WRF-SUEWS. See examples [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/namelist_example/UK) (specially the `sf_surface_physics = 9` which specifies to use SUEWS as the LSM).

9- The rest of steps, are similar to usual WRF runs (running WRF-SUEWS)


Pre-processing
----------------------------------

prepare `wrfinput` using WPS

To generate the original `wrfinput` files (before processing them for WRF-SUEWS), you should follow [here](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/CASES/JAN00/index.php). After generating `wrfinput` and `wrfbdy`, you need to follow pre-processing instructions to modify the input file suitable for WRF-SUEWS runs

Environment to run pre-processors

Use conda to create a fresh environment for this pipeline using [environment.yml](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/blob/master/input-processor/pre-processor-UK/environment.yml):

```bash
conda env create -f environment.yml
```

SUEWS-specific pre-processing

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

`parameters`: modifying parameters related to non-urban areas such as albedo, LAI, conductances based on [Omidvar et al. 2020](https://gmd.copernicus.org/preprints/gmd-2020-148/) - For all domains

`timezone`: modifying grids timezone for all domains (might not work correctly because of the python package problem) - For all domains. It is recommended to specify the time-zone in the SUEWS runs forlder, so the `timezone` variable is assigned correctly in `wrf-input` files


**Each of the above steps is related to a utility function located [here](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/utility)**

**The input files (`wrf-inputs`) as well as non-urban parameters, and templates for `namelist.suews` and `SUEWS_param.json` are located in the [input folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/input)**

**Data related to land cover and building and vegetation height is located in the [Data folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/data)**

**Different versions of `wrfinput` files are saved in the [output folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output) during the run. The final output to be used for WRF-SUEWS runs will be written in the [final folder](https://github.com/Urban-Meteorology-Reading/WRF-SUEWS/tree/master/input-processor/pre-processor-UK/output/final)**

Simulation
------------------------------------
run simulation

After compilation and preparing the inputs, use the following script to run the simulations on JASMIN (go to [WRF-SUEWS directory]/test/em_real):

```
#!/bin/bash
#BSUB -q par-multi
#BSUB -n 30
#BSUB -o %J.out
#BSUB -e %J.err
#BSUB -W 48:00

echo "Running WRF"
mpirun ./wrf.exe
```



Post-processing
----------------------------------

examine output files
