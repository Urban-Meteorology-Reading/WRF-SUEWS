# WRF-SUEWS preprocessor

# TODO #84: to make the WSPS workflow more general (i.e., not too London specific)
# %%
import json
import os
from os.path import isfile
import shutil
from pathlib import Path

from utility.change_to_suews import add_SUEWS_wrfinput_single
from utility.modify_London import modify_all_London
from utility.parameters import parameters
from utility.SUEWS_param import getting_SUEWS_params
from utility.timezone_collector import set_timezone

################################################
# Dashboard for controling the steps: 0=no 1=yes
# TODO #84
steps = {
    "clean_dirs": 1,
    "extract_params_site": 1,
    "extract_params_vegs": 1,
    "extract_params_extra_lands": 1,
    "modify_trans": 1,
    "change_to_SUEWS": 1,
    "modify_London": 0,
    "parameters": 1,
    "timezone": 0,
}
################################################
list_site = ["London", "Swindon"]

# #values_trans = [0.2039, 0.2105] # April
# values_trans = [0.01641, 0.01756] # April
# first_day_str = '2012-04-10' # April

# TODO #84: values_trans should be customisable.

# values_trans = [0.186, 0.0712] # January
values_trans = [0.1257, 0.1216]  # January
str_first_day = "2012-01-10"  # January

# # #values_trans = [0.0781, 0.0294] # July
# values_trans = [0.0528, 0.0097] # July
# first_day_str = '2012-07-15' # July

# #values_trans = [0.1485, 0.0876] # October
# values_trans = [0.2153, 0.01400] # October
# first_day_str = '2012-10-1' # October
################################################
finalize = 0

# TODO #84: input folder should be customisable.
path_dir_input = Path("./sample-case/input").expanduser().resolve()

path_json_prm = path_dir_input / "SUEWS_param.json"
path_csv_phenol = path_dir_input / "phenol_attrs.csv"
path_nml_suews = path_dir_input / "namelist.suews"


# TODO #84: output folder should be customisable.
# path to output root:
path_dir_output = Path("./sample-case/output").expanduser().resolve()

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
        print(f"cleaning {p_dir.as_posix()} . . .")

        if p_dir.exists():
            shutil.rmtree(p_dir)

        # re-create an empty folder as will be needed in following steps
        p_dir.mkdir(parents=True)


################################################
if steps["extract_params_site"] == 1:
    print("\n Extracting SUEWS parameters for cities . . . ")
    for site in list_site:
        print("preparing for " + site + " . . .")
        path_runcontrol = path_dir_input / "runs" / site / "RunControl.nml"
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
    print("\n Extracting SUEWS parameters for vegetations . . . ")
    for veg_type in ["EveTr", "DecTr", "Grass"]:
        print("preparing for " + veg_type + " . . .")
        path_runcontrol = path_dir_input / "runs" / list_site[0] / "RunControl.nml"

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
if steps["extract_params_extra_lands"] == 1:
    print("\n Extracting SUEWS parameters for 4 extera lands . . . ")
    for veg_type in ["G1", "G2", "G3", "G4"]:
        print("preparing for " + veg_type + " . . .")
        path_runcontrol = path_dir_input / "runs" / list_site[1] / "RunControl.nml"
        # path_csv_phenol = path_dir_input / "phenol_attrs.csv"
        # path_json_prm = path_dir_input / "SUEWS_param.json"
        # path_nml_suews = path_dir_input / "namelist.suews"
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
    print("\n Modifying taransmisivity . . .")
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
        print(f"\n Modifying {new_json.as_posix()} . . .")
        with open(new_json, "w") as fp:
            json.dump(vars_to_add, fp, indent=4)
################################################
if steps["change_to_SUEWS"] == 1:
    print("\n Adding SUEWS inputs to WRF inputs . . .")
    path_out = path_dir_output / "1-changed_to_SUEWS"
    list_wrfinput_base = sorted(path_dir_input.glob("wrfinput_d0?"))
    for path_wrfinput in list_wrfinput_base:
        if "d04" in path_wrfinput.name:
            path_json_prm_x = path_dir_output / "SUEWS_param_Swindon.json"
        else:
            path_json_prm_x = path_dir_output / "SUEWS_param_London.json"
        add_SUEWS_wrfinput_single(path_wrfinput, path_json_prm_x, path_out)
    # change_input_to_SUEWS(path_dir_input)
################################################
# all specific modification should be done here!!!
# TODO #84: this looks very specific to London;
# should we include this in the WSPS?
# maybe split this into a separate piece of code
if steps["modify_London"] == 1:
    print("\n Modifying London domain . . .")
    modify_all_London()
    os.remove("output/1-changed_to_SUEWS/wrfinput_d03.suews")
    name1 = "output/1-changed_to_SUEWS/wrfinput_d03.suews.new"
    name2 = "output/1-changed_to_SUEWS/wrfinput_d03.suews"
    os.rename(name1, name2)
################################################
if steps["parameters"] == 1:
    print("\n Modifying parameters . . .")
    # TODO #84
    # this is very specific to Swindon: needs to be corrected for more generic purpose
    path_runcontrol = path_dir_input / "runs" / list_site[1] / "RunControl.nml"
    parameters(path_dir_output, path_csv_phenol, path_runcontrol, str_first_day)
    path_out = path_dir_output / "2-parameters_changed"
    finalize = 1
################################################
if steps["timezone"] == 1:
    print("\n Changing timezone values . . . ")
    set_timezone()
    path_out = path_dir_output / "3-timzone_changed"
    finalize = 1
################################################
if finalize == 1:
    print(f'working on {path_out.as_posix()}')
    src_files = sorted([fn for fn in path_out.glob("*") if fn.is_file()])
    for file_name in src_files:
        # full_file_name = os.path.join(out, file_name)
        # if os.path.isfile(full_file_name):
        print(f"copying {file_name.name} to output/final")
        shutil.copy(file_name, path_dir_output / "final")
