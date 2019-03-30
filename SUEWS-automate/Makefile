
# directory of orginal/official source code
# this dir is included as a git submodule so DON'T make ANY change there
# dir_src_WRF='../WRF'

# directory of SUEWS source code
# this dir is included as a git submodule so DON'T make ANY change there
# dir_src_SUEWS='../SUEWS'

# workding directory for WRF-SUEWS coupling
# to hold all the coupling modifications
# dir_WRF_SUEWS= '../WRFV4.0-SUEWS-coupled'

# TODO:
# suggested workflow:
# 1. make $dir_WRF_SUEWS if not existing
# 2. softlink all files from $dir_src_WRF
# 3. delete the symlinks to files to modify: copy those files
# 4. run script to modify the files
# 5. generate SUEWS related source files from $dir_src_SUEWS and add them to $dir_WRF_SUEWS

main:
	python3 automate_main.py
	cp module_sf_suews.F ./WRF/phys/
	cp module_sf_suewsdrv.F ./WRF/phys/
	cp registry.suews ./WRF/Registry/

# TODO:
# add a `clean` target