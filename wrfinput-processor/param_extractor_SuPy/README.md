# Runnig SUEWS offline, and extracting suitbale SUEWS parameters

- This program runs SUEWS offile until convergence to get suitble initial conditions.

- In addition, it extract suitbale parameters for SUEWS

- Outputs of this script are `SUEWS_param_new.json` and `namelist.suews.new`

- `SUEWS_param_new.json`should be used as an input for `../change_to_SUEWS` pre processor folder

- `namelist.suews.new` is the namelist that should be put in the run folder of WRF-SUEWS
