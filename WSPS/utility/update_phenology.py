#%%
import glob
from pathlib import Path
import geopandas as gpd
from pyproj import Proj, transform
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import xarray as xr
from scipy.interpolate import griddata
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.colors as colors
from .spin_up import spin_lai_albedo
import json


#%%
def mod_gs(ds_base, g1, g2, g3, g4, g5, g6, i, j):

    ds_base["G1"].values[0, i, j] = g1
    ds_base["G2"].values[0, i, j] = g2
    ds_base["G3"].values[0, i, j] = g3
    ds_base["G4"].values[0, i, j] = g4
    ds_base["G5"].values[0, i, j] = g5
    ds_base["G6"].values[0, i, j] = g6

    return ds_base


def change_soil_moisture(ds_base, path_dir_output, str_lc, i, j):
    path_json_prm = path_dir_output / f"SUEWS_param_{str_lc}.json"
    with open(path_json_prm) as param_file:
        params = json.load(param_file)
    ds_base["SoilMoist_SUEWS".upper()].values[0, :, i, j] = params["soilmoist_SUEWS"][
        "value"
    ]
    return ds_base


def mod_lai_albedo(
    ds_base,
    baset,
    basete,
    maxlai,
    minlai,
    maxg,
    maxalb,
    minalb,
    alb_init,
    lai_init,
    i,
    j,
):

    ds_base["MaxConductance_SUEWS".upper()].values[0, :, i, j] = maxg
    ds_base["LaiMax_SUEWS".upper()].values[0, :, i, j] = maxlai
    ds_base["LaiMin_SUEWS".upper()].values[0, :, i, j] = minlai
    ds_base["Lai_SUEWS".upper()].values[0, :, i, j] = lai_init
    ds_base["BaseT_SUEWS".upper()].values[0, :, i, j] = baset
    ds_base["BaseTe_SUEWS".upper()].values[0, :, i, j] = basete

    ds_base["GDDFULL_SUEWS".upper()].values[0, :, i, j] = [1000, 1000, 1000]
    ds_base["SDDFULL_SUEWS".upper()].values[0, :, i, j] = [-1000, -1000, -1000]

    ds_base["albMax_EveTr_SUEWS".upper()].values[0, i, j] = maxalb[0]
    ds_base["albMax_DecTr_SUEWS".upper()].values[0, i, j] = maxalb[1]
    ds_base["albMax_Grass_SUEWS".upper()].values[0, i, j] = maxalb[2]

    ds_base["albMin_EveTr_SUEWS".upper()].values[0, i, j] = minalb[0]
    ds_base["albMin_DecTr_SUEWS".upper()].values[0, i, j] = minalb[1]
    ds_base["albMin_Grass_SUEWS".upper()].values[0, i, j] = minalb[2]

    ds_base["albEveTr_SUEWS".upper()].values[0, i, j] = alb_init[0]
    ds_base["albDecTr_SUEWS".upper()].values[0, i, j] = alb_init[1]
    ds_base["albGrass_SUEWS".upper()].values[0, i, j] = alb_init[2]

    return ds_base


def update_phenology(path_dir_output, path_csv_phenol, path_runcontrol, first_day_str,urban_class_threshold, urban_class):
    p_out = Path(path_dir_output) / "1-changed_to_SUEWS"

    list_path_wrfinput = list(p_out.glob("wrfinput_d0*"))
    list_path_wrfinput.sort()

    # attrbutes related to phenology loaded externally
    phenol_attrs = pd.read_csv(path_csv_phenol)

    maxlai = phenol_attrs["maxlai"].values[0:3]
    minlai = phenol_attrs["minlai"].values[0:3]
    maxg = phenol_attrs["maxg"].values[0:3]
    maxg_crop = phenol_attrs["maxg"].values[4]
    maxalb = phenol_attrs["maxalb"].values[0:3]
    minalb = phenol_attrs["minalb"].values[0:3]
    baset = phenol_attrs["baset"].values[0:3]
    basete = phenol_attrs["basete"].values[0:3]

    alb_init, lai_init = spin_lai_albedo(
        maxalb, minalb, maxlai, minlai, baset, basete, first_day_str, path_runcontrol
    )
    g1 = phenol_attrs["g1"].values
    g2 = phenol_attrs["g2"].values
    g3 = phenol_attrs["g3"].values
    g4 = phenol_attrs["g4"].values
    g5 = phenol_attrs["g5"].values
    # TODO: tl, th, s1 and s2 should be varying at the grid level: currently set for a whole domain
    # tl = phenol_attrs["tl"].values
    # th = phenol_attrs["th"].values
    g6 = phenol_attrs["g6"].values
    # s1 = phenol_attrs["g6"].values*phenol_attrs["smdWP"].values
    # s2 = 0

    for path_wrfinput in list_path_wrfinput:
        print(f"working on {path_wrfinput.name}")
        ds_base = xr.open_dataset(path_wrfinput)
        ds_base_orig = ds_base.copy(deep=True)

        # land cover fractions using SUEWS scheme
        dict_sfr_suews = {}
        dict_sfr_suews["urban"] = 0
        dict_sfr_suews["dectr"] = 0
        dict_sfr_suews["evetr"] = 0
        dict_sfr_suews["grass"] = 0
        dict_sfr_suews["bsoil"] = 0
        dict_sfr_suews["water"] = 0

        # base land cover surface fraction
        lc_sfc_base = ds_base["LANDUSEF"].copy(deep=True)

        # update all SUEWS-based lanc cover fractions
        dict_sfr_suews["urban"] = lc_sfc_base.values[0, 12, :, :]
        for i in [2, 3]:
            dict_sfr_suews["dectr"] = dict_sfr_suews["dectr"] + lc_sfc_base.values[0, i, :, :]
        for i in [0, 1, 4]:
            dict_sfr_suews["evetr"] = dict_sfr_suews["evetr"] + lc_sfc_base.values[0, i, :, :]
        for i in [5, 6, 7, 8, 9, 11, 13]:
            dict_sfr_suews["grass"] = dict_sfr_suews["grass"] + lc_sfc_base.values[0, i, :, :]
        for i in [15, 17, 18, 19]:
            dict_sfr_suews["bsoil"] = dict_sfr_suews["bsoil"] + lc_sfc_base.values[0, i, :, :]
        for i in [10, 16]:
            dict_sfr_suews["water"] = dict_sfr_suews["water"] + lc_sfc_base.values[0, i, :, :]

        for i in range(dict_sfr_suews["urban"].shape[0]):
            for j in range(dict_sfr_suews["urban"].shape[1]):

                mx_fr = np.max(
                    [
                        dict_sfr_suews["urban"][i, j],
                        dict_sfr_suews["evetr"][i, j],
                        dict_sfr_suews["dectr"][i, j],
                        dict_sfr_suews["grass"][i, j],
                        dict_sfr_suews["bsoil"][i, j],
                        dict_sfr_suews["water"][i, j],
                    ]
                )
                ###########################################################################
                not_urban_flag = 1
                for idx,threshold in enumerate(urban_class_threshold):
                    if idx == 0:
                        if dict_sfr_suews["urban"][i, j] > urban_class_threshold[idx]:
                            ds_base = change_soil_moisture(
                                ds_base, path_dir_output, urban_class[idx], i, j
                            )
                            not_urban_flag = 0
                            break
                    else:
                        if (dict_sfr_suews["urban"][i, j] > urban_class_threshold[idx]) and (dict_sfr_suews["urban"][i, j] <= urban_class_threshold[idx-1]):
                            ds_base = change_soil_moisture(
                                ds_base, path_dir_output, urban_class[idx], i, j
                            )
                            not_urban_flag = 0
                            break
                ###########################################################################
                if not_urban_flag:
                    if mx_fr == dict_sfr_suews["evetr"][i, j]:
                        ds_base = mod_gs(
                            ds_base, g1[0], g2[0], g3[0], g4[0], g5[0], g6[0], i, j
                        )
                        ds_base = mod_lai_albedo(
                            ds_base,
                            baset,
                            basete,
                            maxlai,
                            minlai,
                            maxg,
                            maxalb,
                            minalb,
                            alb_init,
                            lai_init,
                            i,
                            j,
                        )
                        ds_base = change_soil_moisture(
                            ds_base, path_dir_output, "EveTr", i, j
                        )
                    ###########################################################################
                    elif mx_fr == dict_sfr_suews["dectr"][i, j]:
                        ds_base = mod_gs(
                            ds_base, g1[1], g2[1], g3[1], g4[1], g5[1], g6[1], i, j
                        )
                        ds_base = mod_lai_albedo(
                            ds_base,
                            baset,
                            basete,
                            maxlai,
                            minlai,
                            maxg,
                            maxalb,
                            minalb,
                            alb_init,
                            lai_init,
                            i,
                            j,
                        )
                        ds_base = change_soil_moisture(
                            ds_base, path_dir_output, "DecTr", i, j
                        )
                    ###########################################################################
                    elif (mx_fr == dict_sfr_suews["grass"][i, j]) and (
                        (lc_sfc_base.values[0, 11, i, j] + lc_sfc_base.values[0, 13, i, j]) >= 0.5
                    ):  # Grass-Crop
                        ds_base = mod_gs(
                            ds_base, g1[4], g2[4], g3[4], g4[4], g5[4], g6[4], i, j
                        )
                        maxg_temp = maxg.copy()
                        maxg_temp[2] = maxg_crop
                        ds_base = mod_lai_albedo(
                            ds_base,
                            baset,
                            basete,
                            maxlai,
                            minlai,
                            maxg_temp,
                            maxalb,
                            minalb,
                            alb_init,
                            lai_init,
                            i,
                            j,
                        )

                        ds_base = change_soil_moisture(
                            ds_base, path_dir_output, "Grass", i, j
                        )
                    ###########################################################################
                    elif (
                        mx_fr == dict_sfr_suews["grass"][i, j]
                        and (lc_sfc_base.values[0, 11, i, j] + lc_sfc_base.values[0, 13, i, j]) < 0.5
                    ):
                        ds_base = mod_gs(
                            ds_base, g1[2], g2[2], g3[2], g4[2], g5[2], g6[2], i, j
                        )
                        ds_base = mod_lai_albedo(
                            ds_base,
                            baset,
                            basete,
                            maxlai,
                            minlai,
                            maxg,
                            maxalb,
                            minalb,
                            alb_init,
                            lai_init,
                            i,
                            j,
                        )
                        ds_base = change_soil_moisture(
                            ds_base, path_dir_output, "Grass", i, j
                            )
                    ###########################################################################
                    elif mx_fr == dict_sfr_suews["bsoil"][i, j]:
                        ds_base = mod_gs(
                            ds_base, g1[3], g2[3], g3[3], g4[3], g5[3], g6[3], i, j
                        )
                        ds_base = mod_lai_albedo(
                            ds_base,
                            baset,
                            basete,
                            maxlai,
                            minlai,
                            maxg,
                            maxalb,
                            minalb,
                            alb_init,
                            lai_init,
                            i,
                            j,
                        )
                        ds_base = change_soil_moisture(
                            ds_base, path_dir_output, "Grass", i, j
                        )
                    ###########################################################################
                    else:
                        pass
                ###########################################################################
        ds_merged = ds_base_orig.update(ds_base)
        for var in ds_merged.data_vars.keys():
            if "coordinates" in ds_merged[var].attrs:
                del ds_merged[var].attrs["coordinates"]

        file_out = path_dir_output / "2-parameters_changed" / path_wrfinput.name

        ds_merged.to_netcdf(file_out, mode="w", format="NETCDF3_64BIT")
        print(f"SUEWS input has been added to: {file_out.as_posix()}")
