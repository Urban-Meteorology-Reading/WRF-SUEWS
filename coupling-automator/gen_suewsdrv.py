#!/usr/bin/env python
#####################################################################
# generator of module_sf_suewsdrv.F by combining source files
# authors:
# Zhenkun Li, lizhenk@yeah.net
# Ting Sun, ting.sun@reading.ac.uk
# history:
# 13 Aug 2018, initial version
#####################################################################

import pandas as pd
import numpy as np
from glob import glob
import os
from copy import copy


def get_file_list(path_Makefile):
    """Short summary.

    Parameters
    ----------
    path_Makefile : string
        path to makefile for dependencies

    Returns
    -------
    List
        a list of dependencies

    """
    # read in makefile source code as pd.Series
    code_raw = pd.read_csv(path_Makefile, header=None, sep=r'\n',
                           engine='python', comment='#').squeeze("columns")
    # clean source code
    code_clean = code_raw.str.replace('\t', ' ', regex=False).str.strip()

    # retrieve lines for dependencies
    # Updated for new SUEWS 2025 Makefile structure
    modules = ['UTILS =', 'PHYS =', 'DRIVER =', 'TEST =', 'WRF =']
    # positions for staring lines
    pos_file_start = []
    for mod in modules:
        matching_lines = code_clean.index[code_clean.str.startswith(mod)]
        if len(matching_lines) > 0:
            pos_file_start.append(matching_lines[0])

    # positions for ending lines (look for empty lines or next section)
    pos_file_end = []
    for i, start in enumerate(pos_file_start):
        if i < len(pos_file_start) - 1:
            pos_file_end.append(pos_file_start[i+1])
        else:
            # For the last section, find the next empty line or section marker
            remaining = code_clean.iloc[start:]
            empty_or_section = remaining.index[
                (remaining == '') |
                remaining.str.startswith('#') |
                remaining.str.contains('=.*\\$', regex=True, na=False)
            ]
            if len(empty_or_section) > 1:
                pos_file_end.append(empty_or_section[1])
            else:
                pos_file_end.append(start + 10)  # default fallback

    # line blocks of groups
    lines_mod = [code_clean.iloc[start:end]
                 for start, end in zip(pos_file_start, pos_file_end)]

    # organise dependencies - handle continuation lines with backslash
    list_mod_files = []
    for mod in lines_mod:
        # Join lines, remove backslashes and extract filenames
        mod_text = ' '.join(mod.str.replace('\\', '', regex=False).values)
        # Split by = and get the right side, then split by spaces
        if '=' in mod_text:
            files_part = mod_text.split('=', 1)[1]
            files = [f.strip() for f in files_part.split() if f.strip() and f.strip().endswith('.o')]
            list_mod_files.append(pd.Series(files))

    # combine all files into one list
    list_files = pd.concat(list_mod_files).reset_index(
        drop=True).str.replace('.o', '.f95', regex=False).tolist()
    return list_files


def merge_source(path_source_dir, path_target):
    """Short summary.

    Parameters
    ----------
    path_source_dir : string
        path to directory of dependencies
    path_target : string
        path for writing out the merged target file

    Returns
    -------
    path_target : Path
        path for writing out the merged target file

    """
    path_Makefile = os.path.join(path_source_dir, 'Makefile')
    # get list of dependencies
    list_files = get_file_list(path_Makefile)

    # Filter out suews_ctrl_ver.f95 as it's auto-generated
    list_files = [f for f in list_files if f != 'suews_ctrl_ver.f95']

    f = open(path_target, 'w')

    # Write a simple version module for WRF coupling
    f.write("MODULE version\n")
    f.write("IMPLICIT NONE\n")
    f.write("CHARACTER(len=90) :: git_commit = 'WRF-SUEWS-2025' \n")
    f.write("CHARACTER(len=90) :: compiler_ver = 'WRF Coupled Version' \n")
    f.write("END MODULE version\n\n")

    for file in list_files:
        # Source files are in the 'src' subdirectory
        fp = open(os.path.join(path_source_dir, 'src', file), 'r')
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

    return path_target


# path settings:
path_source_dir = '../SUEWS/src/suews'
path_target = './module_sf_suewsdrv.F'


# merge files
# path_merged = merge_source(path_source_dir, path_target)
