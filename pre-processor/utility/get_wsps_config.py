from pathlib import Path
import f90nml

class get_wsps_config:
    def __init__(self,nml):
        self.urban_site_spin_up = nml['wsps']['urban_site_spin_up']
        self.veg_site_spin_up = nml['wsps']['veg_site_spin_up']
        self.values_trans = nml['wsps']['transmissivity']
        self.start_date = nml['wsps']['start_date']
        self.urban_domain_number = nml['wsps']['urban_domain_number']
        self.output_file_name = nml['wsps']['output_file_name']
        self.input_file_name = nml['wsps']['input_file_name']
        self.SUEWS_param_template = nml['wsps']['SUEWS_param_template']
        self.phenology_parameters = nml['wsps']['phenology_parameters']
        self.urban_class_threshold = nml['wsps']['urban_class_threshold']
        self.urban_class = nml['wsps']['urban_class']
        self.data_dir = nml['wsps']['data_dir']


if __name__ == "__main__":
    path_dir_input = Path("./sample-case/input").expanduser().resolve()
    path_nml_suews = path_dir_input / "namelist.suews"
    nml = f90nml.read(path_nml_suews)
    wsps_config=get_wsps_config(nml)
    print(wsps_config.phenology_parameters)