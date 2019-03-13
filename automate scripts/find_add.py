import pandas as pd
import numpy as np

filePath = '/Users/hamidrezaomidvar/Dropbox/WRF-SUEWS/automate scripts/'
in_file = 'Registry.EM_COMMON'
query = 'package   ssibscheme     sf_surface_physics==8'
to_add = 'package   suewsscheme    sf_surface_physics==9       -             state:LAI_SUEWS,albDecTr_SUEWS,albEveTr_SUEWS,albGrass_SUEWS,DecidCap_SUEWS,porosity_SUEWS,GDD_SUEWS,HDD_SUEWS,state_SUEWS,soilmoist_SUEWS,surf_var_SUEWS,landusef_SUEWS,alb_SUEWS,emis_SUEWS,qn1_av_SUEWS,qn1_s_SUEWS,dqndt_SUEWS,dqnsdt_SUEWS,MeltWaterStore,SnowAlb,WUDay,z0m_in,zdm_in'

print('Reading file:'+ in_file)
with open(filePath+in_file,'r') as ifile: # Reading the file
    buf=ifile.readlines()

out_file=in_file+'_mod'

print('Writing file:'+out_file)
with open(filePath+out_file,'w') as ofile:
    for line in buf:
        if(query in line):
            line=line+'\n'+to_add+'\n'
        ofile.write(line)

print('Modifiying file='+in_file+' is finished')