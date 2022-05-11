# WRF-SUEWS Pre-processor (WSPS)

## Steps in summary

1. Use `conda` to create a fresh environment use `environment.yml`:

```bash
conda env create -f environment.yml
```

2. Set up WSPS configuration in the `wsps` section of `namelist.suews`, and add SUEWS related run files associated with the configuration in the [spin up folder](./sample-case/input/spin_ups). You also need to add all `wrfinput_d0*.nc` files from WPS process to your [`input` folder](./sample-case/input)

3.  run:
```shell
python wsps.py
```
4. The modified `wrfinput_d0*.nc` files will be in the generated output folder `output/final`. 

5. (*optional*) If you wish to have site-specific modification of `wrfinput.nc` files (e.g. land cover, population density etc.), you can use `wps_site_specific.py` script in conjunction with customised related modules in [`utility` folder](./utility/site_specific) (see examples of London and Swindon):
```shell
python wsps_site_specific.py
```

## Steps in detail
### Mandatory steps (general)

#### Configuration
In the first step, the WSPS needs to be configured in  `wsps` section of `namelist.suews` (in the root directory of pre processor folder). Please read below to see how to modify configuration (and add necessary SUEWS related files) for your purpose:

```
&wsps
urban_site_spin_up    = 'London', 'Swindon'
urban_domain_number   = '03',     '04'
urban_class_threshold = 0.6,      0.16
urban_class           = 'London', 'Swindon'
veg_site_spin_up      = 'Swindon'
transmissivity        = 0.1257,   0.1216
start_date            = '2012-01-10'
output_file_name      = 'output'
input_file_name       = 'input'
SUEWS_param_template  = 'SUEWS_param.json'
phenology_parameters  = 'phenol_attrs.csv'
data_dir              = 'data'
/

```
Here are the explanation of each option:

- `urban_site_spin_up`: this is the list of the urban site you like to use for spin up. For each of the name in this list, there should be a same-named folder with SUEWS related files to run SuPy in the [spin up folder](./sample-case/input/spin_ups).

- `urban_domain_number`: this is the domain associated with each of sites in `urban_site_spin_up`

- `urban_class_threshold` and `urban_class` : these are threshold related to urban classes (urban fraction) and the associated site for each one. You can have as many threshold as you want. For example, in the above script, the thresholds are `1 < f < 0.6` is assigned to `London`, `0.6 < f < 0.16` is assigned to `Swindon`, and the rest are automatically assigned to vegetated classes.

- `veg_site_spin_up`: this is the site configuration you need to use for vegetated spin up. It can be any configuration as long as there is a same-named folder with SUEWS related files to run SuPy in the [spin up folder](./sample-case/input/spin_ups). In the example above, `Swindon` site configuration is used.


- `transmissivity`: The values for transmissivity correction for each urban site. Put them 0 if no correction is needed.

- `start_date`: Start date of the run

- `output_file_name`: the output file name that will be generated in [sample case folder](./sample-case)

- `input_file_name`: the output file name contains necessary inputs in [sample case folder](./sample-case)

- `SUEWS_param_template`: the name of `json` file containing all the SUEWS variables needed to be added to WRF inputs (it is in the input folder).

- `phenology_parameters`: The name of the file containing parameters related to non-urban areas such as albedo, LAI, conductances based on [Omidvar et al. 2020](https://gmd.copernicus.org/preprints/gmd-2020-148/) (it is in the input folder)

- `data_dir`: name of the data directory necessary for site_specific modifications (it is in the [sample case folder](./sample-case))

### Site specific steps

If you wish to have site-specific modification of `wrfinput.nc` files (e.g. land cover, population density etc.), you can use `wps_site_specific.py` script in conjunction with customised related modules in [`utility` folder](./utility).
You need to follow [this](./utility/site_specific/modify_template.py) template for writing the script for modification of your own site. see examples of [London](./utility/site_specific/modify_London.py) and [Swindon](./utility/site_specific/modify_Swindon.py) in the [`utility` folder](./utility/site_specific).
