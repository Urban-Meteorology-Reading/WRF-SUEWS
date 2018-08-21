# retrive Makefile info
# Ting Sun, ting.sun@reading.ac.uk
# 12 Aug 2018
import pandas as pd
import numpy as np
from glob import glob
import os
from copy import copy

os.getcwd()

path_code = '/Users/sunt05/Documents/WRF-SUEWS/SUEWS/SUEWS-SourceCode'

fl = glob(path_code + '/include.common')
path_Makefile = fl[0]

code_raw = pd.read_csv(path_Makefile, header=None, sep='\n',
                       engine='python', comment='#', squeeze=True)
# code_raw.str.strip()
code_clean = code_raw.str.replace('\t', ' ', regex=False).str.strip()

modules = ['UTILS', 'MODULES', 'OTHERS', 'TEST', 'WRF']
pos_file_start = [code_clean.index[code_clean.str.startswith(mod)][0]
                  for mod in modules]
pos_file_end = copy(pos_file_start[1:]) + [pos_file_start[-1] + 1]
lines_mod = [code_clean.iloc[start:end]
             for start, end in zip(pos_file_start, pos_file_end)]


mod_files = [mod.str.replace('\\', '').str.split('=').sum()
             for mod in lines_mod]
list_mod_files = [pd.Series(mod[1:]).str.strip() for mod in mod_files]

files_all = pd.concat(list_mod_files).reset_index(
    drop=True).str.replace('.o', '.f95', regex=False).tolist()


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
    code_raw = pd.read_csv(path_Makefile, header=None, sep='\n',
                           engine='python', comment='#', squeeze=True)
    # clean source code
    code_clean = code_raw.str.replace('\t', ' ', regex=False).str.strip()

    # retrieve lines for dependencies
    modules = ['UTILS', 'MODULES', 'OTHERS', 'TEST', 'WRF']
    # positions for staring lines
    pos_file_start = [code_clean.index[code_clean.str.startswith(mod)][0]
                      for mod in modules]
    # positions for ending lines
    pos_file_end = copy(pos_file_start[1:]) + [pos_file_start[-1] + 1]

    # line blocks of groups
    lines_mod = [code_clean.iloc[start:end]
                 for start, end in zip(pos_file_start, pos_file_end)]

    # organise dependencies as dicts for groups
    mod_files = [mod.str.replace('\\', '').str.split('=').sum()
                 for mod in lines_mod]
    list_mod_files = [pd.Series(mod[1:]).str.strip() for mod in mod_files]

    # combine all files into one list
    files_all = pd.concat(list_mod_files).reset_index(
        drop=True).str.replace('.o', '.f95', regex=False).tolist()
    return files_all


get_file_list(path_Makefile)


#
