import pandas as pd
import numpy as np
import sub_modules_automate as sa

filePath = '/Users/hamidrezaomidvar/Dropbox/WRF-SUEWS/automate scripts/'
in_file = 'Registry.EM_COMMON'
query = 'package   ssibscheme     sf_surface_physics==8'
to_add = 'package   suewsscheme    sf_surface_physics==9       -             state:LAI_SUEWS,albDecTr_SUEWS,albEveTr_SUEWS,albGrass_SUEWS,DecidCap_SUEWS,porosity_SUEWS,GDD_SUEWS,HDD_SUEWS,state_SUEWS,soilmoist_SUEWS,surf_var_SUEWS,landusef_SUEWS,alb_SUEWS,emis_SUEWS,qn1_av_SUEWS,qn1_s_SUEWS,dqndt_SUEWS,dqnsdt_SUEWS,MeltWaterStore,SnowAlb,WUDay,z0m_in,zdm_in'

sa.find_add(filePath,in_file,query,to_add)

