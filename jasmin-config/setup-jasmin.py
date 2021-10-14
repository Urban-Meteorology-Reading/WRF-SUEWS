# load packages
from pathlib import Path
import time
import pandas as pd
from shutil import copytree, copy

##########################################
# essential paths
# 1. base `run` directory of WRF
p_dir_baserun = Path("../compilation-20211012/run")

# 2. pre-processed wrfinput and other files (wrfbdy and namelist.input) for WRF-SUEWS
p_dir_wrfinput = Path("~/input-WRF-SUEWS").expanduser()

# 3. namelist for WRF-SUEWS
p_namelist_suews = Path("../WSPS/namelist.suews")

# 4. jasmin job script
p_sbatch = Path("./sb-run_wrf.sh")

# 5. base simulation root
today = time.strftime("%Y%m%d")
p_dir_basesim = Path(f"/work/scratch-nopw/sunt05/sim_WRF-SUEWS/sim-{today}")
##########################################

##########################################
# content checking
# check if wrf.exe exist
p_exe_wrf = p_dir_baserun / "wrf.exe"
if not p_exe_wrf.exists():
    raise RuntimeError(f"wrf.exe does not exist under {p_dir_baserun.as_posix()}!")

# check if preprocessed wrfinput are available
list_dir_wrfinput = [
    p_dir for p_dir in sorted(p_dir_wrfinput.glob("*")) if p_dir.is_dir()
]
if list_dir_wrfinput:
    for p_dir in list_dir_wrfinput:
        print(f"checking {p_dir}:")
        for str_pattern in ["wrfinput*","wrfbdy*","namelist*"]:
            list_fn=sorted(p_dir.glob(str_pattern))
            if list_fn:
                print(list_fn)
            else:
                raise RuntimeError(f"{p_dir.as_posix()} doesn't have files matching {str_pattern}")
else:
    raise RuntimeError(f"{p_dir_wrfinput.as_posix()} is empty!")

# check if job file exists
if not p_sbatch.exists():
    raise RuntimeError(f"job file {p_sbatch.as_posix()} does not exist!")
##########################################


##########################################
# create working directory
if p_dir_basesim.exists():
    pass
else:
    print(f"{p_dir_basesim} does not exist: creating it now ...")
    p_dir_basesim.mkdir(exist_ok=True,parents=True)

# copy files
for p_dir_wrfinput in list_dir_wrfinput:
    str_sim = p_dir_wrfinput.name
    print(f"working on {str_sim}")
    # create sim directory using the same
    p_dir_sim = p_dir_basesim / str_sim

    # copy base run files
    copytree(p_dir_baserun, p_dir_sim)

    # copy processed wrfinput files
    for fn in p_dir_wrfinput.glob('wrfinput*.suews'):
        copy(fn, p_dir_sim/fn.name.replace('.suews',''))

    # copy wrfbdy files
    for fn in p_dir_wrfinput.glob('wrfbdy*'):
        copy(fn, p_dir_sim/fn.name)

    # overwrite namelist.input
    for fn in p_dir_wrfinput.glob('namelist.input'):
        copy(fn, p_dir_sim/fn.name)

    # copy job file
    copy(p_sbatch, p_dir_sim)
##########################################

##########################################
