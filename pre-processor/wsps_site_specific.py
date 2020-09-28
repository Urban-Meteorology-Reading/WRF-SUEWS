
from pathlib import Path
import os
import f90nml

from utility.site_specific.modify_London import modify_London
from utility.site_specific.modify_Swindon import modify_Swindon
from utility.get_wsps_config import get_wsps_config

def sub_files(path_dir_output,file_to_change):
    # Renaming the changed input file to be consistent with others
    os.remove(path_dir_output / f"final/{file_to_change.name}")
    name1 = path_dir_output / f"final/{file_to_change.name}.new"
    name2 = path_dir_output / f"final/{file_to_change.name}"
    os.rename(name1, name2)
################################################################
path_nml_suews = Path('.') / "namelist.suews"
nml = f90nml.read(path_nml_suews)
wsps_config=get_wsps_config(nml)
path_dir_output = Path("./sample-case/"+wsps_config.output_file_name).expanduser().resolve()
path_dir_data = Path("./sample-case/"+wsps_config.data_dir).expanduser().resolve()
################################################################

#London
print("\n Modifying London domain . . .")
file_to_change=Path("final/wrfinput_d03.suews")
modify_London(path_dir_output,path_dir_data,file_to_change)
sub_files(path_dir_output,file_to_change)
################################################################

#Swindon
print("\n Modifying Swindon domain . . .")
file_to_change=Path("final/wrfinput_d04.suews")
modify_Swindon(path_dir_output,path_dir_data,file_to_change)
sub_files(path_dir_output,file_to_change)