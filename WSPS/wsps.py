# WRF-SUEWS preprocessor
# %%
import json
import os
from os.path import isfile
import shutil
from pathlib import Path
import f90nml

from utility.change_to_suews import add_SUEWS_wrfinput_single
from utility.update_phenology import update_phenology
from utility.SUEWS_param import getting_SUEWS_params
from utility.timezone_collector import set_timezone
from utility.get_wsps_config import get_wsps_config

################################################
# DON'T MODIFY
# for internal debugging use
# Dashboard for controlling the steps:
# 0=no 1=yes
steps = {
    "clean_dirs": 1,  # clean all output directories
    "extract_params_site": 1,  # spin up for sites
    "extract_params_vegs": 1,  # spin up for vegs
    "modify_trans": 1,  # changing transmissivity
    "change_to_SUEWS": 1,  # update wrfinput
    "update_phenology": 1,  # update phenology
    "timezone": 0,  # change timezone
}
################################################
# path to output and input root:
path_nml_suews = Path(".") / "namelist.suews"
nml = f90nml.read(path_nml_suews)
wsps_config = get_wsps_config(nml)

# name of the output folder
output_dir = wsps_config.output_file_name

# name of the output folder
input_dir = wsps_config.input_file_name

path_dir_output = Path("./sample-case/" + output_dir).expanduser().resolve()
path_dir_input = Path("./sample-case/" + input_dir).expanduser().resolve()

# path to SUEWS template variables json
path_json_prm = path_dir_input / wsps_config.SUEWS_param_template

# path to phenology parameters csv file
path_csv_phenol = path_dir_input / wsps_config.phenology_parameters

# list of urban sites
list_site = wsps_config.urban_site_spin_up
list_site = list_site if isinstance(list_site, list) else [list_site]

# to make sure we pass the list of sites as a list even when there is one
if type(list_site) is list:
    pass
else:
    list_site = [list_site]

# domain number related to urban site
urban_domain_number = wsps_config.urban_domain_number

# the site to use for vegetation spin up
site_veg = wsps_config.veg_site_spin_up

# transmissivity values for urban sites
values_trans = wsps_config.values_trans

# to make sure we pass the list of values of transmissivity as a list even when there is one
if type(values_trans) is list:
    pass
else:
    values_trans = [values_trans]

# start day of the run
str_first_day = wsps_config.start_date

# land cover threshold of impervious surfaces for urban classes
urban_class_threshold = wsps_config.urban_class_threshold

# urban classes
urban_class = wsps_config.urban_class
################################################
finalize = 0
if steps["clean_dirs"] == 1:
    list_dir = [
        "1-changed_to_SUEWS",
        "2-parameters_changed",
        "3-timezone_changed",
        "final",
    ]
    for dir_path in list_dir:
        # full path to work on
        p_dir = path_dir_output / dir_path
        print(f"cleaning {p_dir.as_posix()} ...")

        if p_dir.exists():
            shutil.rmtree(p_dir)

        # re-create an empty folder as will be needed in following steps
        p_dir.mkdir(parents=True)
################################################
if steps["extract_params_site"] == 1:
    print("\n Extracting SUEWS parameters for cities ... ")
    for site in list_site:
        print(f"preparing for {site} ...")
        path_runcontrol = path_dir_input / "spin_ups" / site / "RunControl.nml"
        getting_SUEWS_params(
            path_runcontrol,
            path_csv_phenol,
            path_json_prm,
            path_nml_suews,
            path_dir_output,
            str_first_day,
        )
################################################
if steps["extract_params_vegs"] == 1:
    print("\n Extracting SUEWS parameters for vegetations ... ")
    for veg_type in ["EveTr", "DecTr", "Grass"]:
        print("preparing for " + veg_type + " ...")
        path_runcontrol = path_dir_input / "spin_ups" / site_veg / "RunControl.nml"
        getting_SUEWS_params(
            path_runcontrol,
            path_csv_phenol,
            path_json_prm,
            path_nml_suews,
            path_dir_output,
            str_first_day,
            veg_spin=1,
            veg_type=veg_type,
        )
################################################
if steps["modify_trans"] == 1:
    print("\n Modifying taransmisivity ...")
    for site, value in zip(list_site, values_trans):
        try:
            with open(path_dir_output / f"SUEWS_param_{site}.json") as var_json:
                vars_to_add = json.load(var_json)
                vars_to_add["transdiff_SUEWS"]["value"] = [value]
        except FileNotFoundError as err:
            with open(path_json_prm) as var_json:
                vars_to_add = json.load(var_json)
                vars_to_add["transdiff_SUEWS"]["value"] = [value]

        # new_json = "output/SUEWS_param_" + site + ".json"
        new_json = path_dir_output / f"SUEWS_param_{site}.json"
        print(f"\n Modifying {new_json.as_posix()} ...")
        with open(new_json, "w") as fp:
            json.dump(vars_to_add, fp, indent=4)
################################################
if steps["change_to_SUEWS"] == 1:
    print("\n Adding SUEWS inputs to WRF inputs ...")
    path_out = path_dir_output / "1-changed_to_SUEWS"
    list_wrfinput_base = sorted(path_dir_input.glob("wrfinput_d0?"))

    for path_wrfinput in list_wrfinput_base:

        flag_not_urban_domain = 1
        for domain_n, urban_site in zip(urban_domain_number, list_site):
            if domain_n in path_wrfinput.name:
                path_json_prm_x = path_dir_output / f"SUEWS_param_{urban_site}.json"
                flag_not_urban_domain = 0
                add_SUEWS_wrfinput_single(path_wrfinput, path_json_prm_x, path_out)

        if flag_not_urban_domain:
            path_json_prm_x = path_dir_output / f"SUEWS_param_{list_site[0]}.json"
            add_SUEWS_wrfinput_single(path_wrfinput, path_json_prm_x, path_out)
################################################
if steps["update_phenology"] == 1:
    print("\n Modifying phenology ...")
    path_runcontrol = path_dir_input / "spin_ups" / site_veg / "RunControl.nml"
    update_phenology(
        path_dir_output,
        path_csv_phenol,
        path_runcontrol,
        str_first_day,
        urban_class_threshold,
        urban_class,
    )
    path_out = path_dir_output / "2-parameters_changed"
    finalize = 1
################################################
if steps["timezone"] == 1:
    print("\n Changing timezone values ...")
    set_timezone(path_dir_output, str_first_day)
    path_out = path_dir_output / "3-timzone_changed"
    finalize = 1
################################################
if finalize == 1:
    print(f"working on {path_out.as_posix()}")
    src_files = sorted([fn for fn in path_out.glob("*") if fn.is_file()])
    for file_name in src_files:
        print(f"copying {file_name.name} to {output_dir}/final")
        shutil.copy(file_name, path_dir_output / "final")
