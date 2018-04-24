# -*- makefile -*-
.PHONY:main,clean,test,pip

# OS-specific configurations
ifeq ($(OS),Windows_NT)
	PYTHON_exe = /c/Users/sunt05/Anaconda2/python.exe
	# F2PY_PY= /c/Users/sunt05/Anaconda2/Scripts/f2py.py
	# F2PY_EXE = $(PYTHON) $(F2PY_PY)
	TARGET=$(MODULE).pyd
else
	UNAME_S := $(shell uname -s)
	TARGET=$(MODULE).so

	ifeq ($(UNAME_S),Linux) # Linux
		PYTHON_exe=python
		# F2PY_EXE = f2py
	endif

	ifeq ($(UNAME_S),Darwin) # macOS
		PYTHON_exe=python
		# F2PY_EXE = f2py
	endif

endif

MODULE=SUEWS_driver

SUEWS_dir = SUEWS-SourceCode

PYTHON := $(if $(PYTHON_exe),$(PYTHON_exe),python)
# All the files which include modules used by other modules (these therefore
# needs to be compiled first)
FILES = LUMPS_Module_constants.f95  \
				SUEWS_driver.f95


main:
	$(MAKE) -C $(SUEWS_dir) clean; # clean Fortran SUEWS build
	$(MAKE) -C $(SUEWS_dir) main; # make SUEWS with the `main` recipe
	$(PYTHON) setup.py bdist_wheel # all f2py compilation is done by `setup.py`
	-rm -rf *.o *.mod *.f95 *.a *.dSYM


main exe:
	$(MAKE) -C $(SUEWS_dir) clean; # clean Fortran SUEWS build
	$(MAKE) -C $(SUEWS_dir) main; # make SUEWS with the `main` recipe
	-rm -rf *.o *.mod *.f95 *.a *.dSYM


# If wanted, clean all *.o files after build
clean:
	 -rm -rf *.o *.mod *.dSYM $(TARGET) SuPy/$(MODULE).*;
	 -$(PYTHON) setup.py clean --all

# clean all existing builds, rebuild f2py libs, build wheels and test
test:
	$(MAKE) clean;
	$(MAKE) main;
	$(PYTHON) setup.py test

# clean all existing builds, rebuild f2py libs, build wheels and submit
pip:
	$(MAKE) clean;
	$(MAKE) main;
	$(PYTHON) setup.py bdist_wheel upload
