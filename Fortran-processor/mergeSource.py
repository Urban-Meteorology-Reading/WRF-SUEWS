#!/usr/bin/env python
import os


UTILS =   ['suews_ctrl_const.f95',
           'suews_util_stringmod.f95',
           'suews_util_qsort.f95',
           'suews_util_time.f95',
           'suews_util_meteo.f95',
           'suews_util_datetime.f95',
           'suews_util_minpack.f95']
MODULES = ['suews_phys_narp.f95',
           'suews_ctrl_input.f95',
           'suews_phys_atmmoiststab.f95',
           'suews_phys_bluews.f95',
           'suews_phys_waterdist.f95',
           'suews_phys_snow.f95',
           'suews_phys_dailystate.f95',
           'suews_phys_estm.f95',
           'suews_ctrl_output.f95',
           'suews_phys_anohm.f95',
           'suews_ctrl_driver.f95']
OTHERS =  ['suews_ctrl_translate.f95',
           'suews_phys_lumps.f95',
           'suews_phys_resist.f95',
           'suews_ctrl_error.f95',
           'suews_phys_evap.f95',
           'suews_ctrl_init.f95',
           'suews_phys_anemsn.f95',
           'suews_phys_biogenco2.f95']
TEST =    ['suews_phys_ohm.f95',
           'suews_ctrl_calculations.f95']
WRF =     ['suews_ctrl_sumin.f95']

MODs = [UTILS, MODULES, OTHERS, TEST, WRF]

fileList = []

for Mod in MODs:
    fileList.extend(Mod)

# fileList = UTILS + MODULES + OTHERS + TEST + WRF

f = open(os.path.join('./', 'module_sf_suewsdrv.F'), 'w')
for file in fileList:
    fp = open(os.path.join('./', file), 'r')
    line = fp.readline()
    while line:
        # check if define wrf
        if line.lstrip().startswith('#ifdef wrf'):
            line = fp.readline()
            break_flag = False
            while break_flag == False:
                if line.lstrip().startswith('#else'):
                    line = fp.readline()
                    while break_flag == False:
                        if line.lstrip().startswith('#endif'):
                            break_flag = True
                        else:
                            line = fp.readline()
                elif line.lstrip().startswith('#endif'):
                    break_flag = True
                else:
                    f.writelines(line)
                line = fp.readline()
        # check if define nc
        elif line.lstrip().startswith('#ifdef nc'):
            line = fp.readline()
            break_flag = False
            while break_flag == False:
                if line.lstrip().startswith('#else'):
                    line = fp.readline()
                    while break_flag == False:
                        if line.lstrip().startswith('#endif'):
                            break_flag = True
                        else:
                            f.writelines(line)
                            line = fp.readline()
                elif line.lstrip().startswith('#endif'):
                    break_flag = True
                line = fp.readline()
        else:
            f.writelines(line)
            line = fp.readline()
    fp.close()
    f.writelines('\n')
f.close()
