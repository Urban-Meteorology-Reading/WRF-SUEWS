# %%
import pandas as pd
from datetime import datetime
import f90nml
import numpy as np
import supy as sp
from pathlib import Path
import json
from .modify_df_state import modify_attr

print(f"supy version is : {sp.__version__}")


def spinup_SUEWS(
    path_runcontrol, path_csv_phenol, first_day_str, veg_spin=0, veg_type=""
):

    print("Initializing SUEWS variables.....")

    df_state_init = sp.init_supy(path_runcontrol)

    df_phenol = pd.read_csv(path_csv_phenol)

    if veg_spin == 1:
        print("Initializing for " + veg_type)
        df_state_init = modify_attr(df_state_init, df_phenol, veg_type)

    grid = df_state_init.index[0]
    df_forcing = sp.load_forcing_grid(path_runcontrol, grid)
    first_day = datetime.strptime(first_day_str, "%Y-%m-%d")

    print("Rotating the time based on the first day of " + first_day_str)
    first_part = df_forcing.loc[df_forcing[df_forcing.index >= first_day].index]
    second_part = df_forcing.loc[
        df_forcing[df_forcing.index < first_day - pd.Timedelta("1 days")].index
    ]
    second_part.index = second_part.index + pd.Timedelta("366 days")

    df_forcing_2 = first_part.append(second_part)
    df_forcing_2.iy = df_forcing_2.index.year
    df_forcing_2.index.freq = first_part.index.freq
    round_number = 0
    error = 0.4
    counter = 0
    while error >= 0.1 or counter < 3:
        counter += 1
        round_number = round_number + 1
        print("Running SUEWS for round number " + str(round_number) + ".....")
        df_output, df_state_final = sp.run_supy(
            df_forcing_2, df_state_init, save_state=False
        )
        final_state = df_state_final[df_state_init.columns.levels[0]].iloc[1]
        df_state_init.iloc[0] = final_state
        soilstore_before = df_state_final.soilstore_id.iloc[0]
        soilstore_after = df_state_final.soilstore_id.iloc[1]
        diff_soil = sum(abs(soilstore_after - soilstore_before))
        error = 100 * diff_soil / soilstore_before.mean()
        print(f"Change in soil store in % = {error:.2f}")

    print(f"SUEWS spinup is finished in {round_number} iterations.")

    return df_state_init


def getting_SUEWS_params(
    path_runcontrol,
    path_csv_phenol,
    path_json_prm,
    path_nml_suews,
    path_dir_out,
    str_first_day,
    veg_spin=0,
    veg_type="",
):
    if veg_spin == 1:
        print("Spining up for vegetation . . .")
        df_state_init = spinup_SUEWS(
            path_runcontrol,
            path_csv_phenol,
            str_first_day,
            veg_spin=1,
            veg_type=veg_type,
        )
    else:
        df_state_init = spinup_SUEWS(path_runcontrol, path_csv_phenol, str_first_day)

    print("Putting NetRadiationMethod = 1")
    df_state_init.netradiationmethod = 1

    int_list_method = [
        "snowuse",
        "roughlenmommethod",
        "roughlenmommethod",
        "emissionsmethod",
        "netradiationmethod",
        "storageheatmethod",
        "ohmincqf",
        "startdls",
        "enddls",
        "laitype",
    ]

    for int_var in int_list_method:
        df_state_init[int_var] = df_state_init[int_var].astype(int)

    df_state_init["snowuse"] = int(
        0
    )  # remove this if snowuse is implemented otherwise it cause problems during winter

    df_state_init.rename(columns={"soilstore_id": "soilmoist"}, inplace=True)
    ##################### JSON ######################################

    with open(path_json_prm) as suews_file:
        suews_params = json.load(suews_file)

    for key in suews_params.keys():

        param = key.split("_SUEWS")[0].lower()
        df_columns = df_state_init.columns

        if param in df_columns:
            suews_params[key]["value"] = list(df_state_init[param].iloc[0])
        elif param + "_id" in df_columns:
            suews_params[key]["value"] = list(df_state_init[param + "_id"].iloc[0])
        else:
            print("Parameter " + '"' + param + '"' + " was not found in df_state")

    path_out = Path(path_dir_out)
    str_site = Path(path_runcontrol).parent.name
    if veg_spin == 1:
        new_json = path_out / f"SUEWS_param_{veg_type}.json"
    else:
        new_json = path_out / f"SUEWS_param_{str_site}.json"
    print(f"creating {new_json.as_posix()} ")
    with open(new_json, "w") as fp:
        json.dump(suews_params, fp, indent=4)

    ##################### NAMELIST ######################################

    nml = f90nml.read(path_nml_suews)
    df_columns = df_state_init.columns

    profiles = {
        "ahprof_24hr",
        "humactivity_24hr",
        "popprof_24hr",
        "traffprof_24hr",
        "wuprofa_24hr",
        "wuprofm_24hr",
        "snowprof_24hr",
        "laipower",
    }

    coefs = {"ohm_coef", "waterdist"}

    patch_change = {"coeff", "method"}

    for key in patch_change:

        for item in nml[key].items():

            value = df_state_init[item[0]].iloc[0]

            if item[0] in coefs:

                dim_str = df_state_init[item[0]].columns[-1][1:-1]
                dims = tuple(int(s) + 1 for s in dim_str.split(", ") if s.isdigit())
                order = "C"

                new_value = [
                    l.tolist() for l in list(np.reshape(list(value), dims, order=order))
                ]

            elif item[0] in profiles:

                dim_str = df_state_init[item[0]].columns[-1][1:-1]
                dims = tuple(int(s) + 1 for s in dim_str.split(", ") if s.isdigit())[
                    ::-1
                ]
                new_value = [
                    l.tolist() for l in list(np.reshape(list(value), dims, order="F"))
                ]

            else:

                new_value = list(value)

            nml[key][item[0]] = new_value
    if veg_spin != 1:
        new_nml = path_out/f"namelist_{str_site}.suews"
        print(f"creaitng {new_nml.as_posix()}")
        nml.write(new_nml, force=True)
