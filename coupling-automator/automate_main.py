#!/usr/bin/env python3
# %%

import json
import os
from pathlib import Path
from shutil import copy, copytree, rmtree

import numpy as np
import pandas as pd
# os.chdir('./automator')
from gen_suewsdrv import merge_source

# os.chdir('../automator')
os.getcwd()


# %%

def find_add(path_file, in_file, query, to_add):

    print('Reading file:' + in_file)
    with open(path_file/in_file, 'r') as ifile:  # Reading the file
        buf = ifile.readlines()

    out_file = in_file

    print('Writing file:'+out_file)
    with open(path_file/out_file, 'w') as ofile:
        for line in buf:
            if(query in line):
                # do this later on `to_add` as a list to store multiline strings
                # line = line+'\n'.join(to_add)+'\n'
                line = line+to_add+'\n'
            ofile.write(line)

    print(f'Modifying {in_file} is finished\n')


with open('changes_list.json') as fn_json:
    change_list = json.load(fn_json)
# change_list
# %%
# directory of orginal/official source code
# this dir is included as a git submodule so DON'T make ANY change there
path_src_WRF = Path('../WRF')


# %%
# directory of SUEWS source code
# this dir is included as a git submodule so DON'T make ANY change there
path_src_SUEWS = Path('../SUEWS/SUEWS-SourceCode')

# %%
# working directory for WRF-SUEWS coupling
# to hold all the coupling modifications
path_working = Path('../xx-test-xx')

# %%
# suggested workflow:
# 1. make $dir_WRF_SUEWS if not existing
if path_working.exists():
    rmtree(path_working)

# %%
# 2. softlink all files/dirs from `path_src_WRF` to `path_working`
# except for those dot files
# import os
# os.chdir(path_working)
# Path().cwd()
# for sub in Path(path_src_WRF).glob('*'):
#     if not sub.name.startswith('.'):
#         print(sub.name)
#         sub_target = path_working/(sub.name)
#         print(sub_target, sub_target.exists())
#         # force link by deleting existing
#         if sub_target.exists():
#             sub_target.unlink()
#         sub_target.symlink_to(sub)

copytree(path_src_WRF, path_working, ignore_dangling_symlinks=True)

# %%
# 3.1 delete the symlinks to files to modify: copy those files
# 3.2 then run script to modify the files


for in_file in change_list.keys():
    path_file = path_working/change_list[in_file]["filePath"]
    # path_file = path_file.resolve()
    path_file_WRF = path_src_WRF/change_list[in_file]["filePath"]
    print(path_file,'existing?' ,path_file.exists())

    # if path_file.is_symlink():
    #     path_file.unlink()
    #     copytree(path_file_WRF, path_file)
    #     print(path_file.resolve())

    query = change_list[in_file]["query"]
    for qkey, qval in query.items():
        find_add(path_file, in_file, qkey, qval)


# %%
# 4. generate SUEWS related source files from $dir_src_SUEWS and add them to $dir_WRF_SUEWS
path_sf_suewsdrv = path_working/'phys'/'module_sf_suewsdrv.F'
merge_source(path_src_SUEWS, path_sf_suewsdrv)
# path_sf_suewsdrv


# %%
# 5. copy SUEWS wrapper and registry files to WRF directory
list_file_to_copy=[
    # (file to copy, destination in working folder)
    ('module_sf_suews.F','phys'),
    ('registry.suews','Registry'),
]
for file, dst in list_file_to_copy:
    copy(file, path_working/dst)
    file_copied = path_working/dst/file
    print(file_copied, 'copied?', file_copied.exists())



# %%
