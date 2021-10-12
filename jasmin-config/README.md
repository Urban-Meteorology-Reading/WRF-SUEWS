# Configuration on JASMIN

[JASMIN](https://www.ceda.ac.uk/services/jasmin/) is the main cluster for performing WRF-SUEWS simulations.

## Compilation

see [here](../README.md#steps)

## Pre-processing

Original `wrfinput` files should be pre-processed by [WSPS](../WSPS) before it can be used by WRF-SUEWS: more details can be found in the [README](../WSPS/README.md) under [WSPS](../WSPS).

## Simulation

1. run `python setup-jasmin.py` to set up simulation folders; more setting details can be found in the [`setup-jasmin.py`](./setup-jasmin.py); related paths should be set in its first section.

2. go to simulation directory as set in `p_dir_basesim` of [`setup-jasmin.py`](./setup-jasmin.py) and submit job file by `sbatch sb-run_wrf.sh`.