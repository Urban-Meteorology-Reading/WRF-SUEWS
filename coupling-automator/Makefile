# WRF-SUEWS Coupling Automator Makefile
# Orchestrates the library-based coupling workflow

.PHONY: all help check-lib build-lib clean patch

# Default target: set up WRF-SUEWS coupling
all: check-lib
	@echo ""
	@echo "=========================================="
	@echo " Running coupling automation..."
	@echo "=========================================="
	python3 ./automate_main.py

# Check if SUEWS library exists (non-fatal, just warns)
check-lib:
	@if [ ! -f ../SUEWS/lib/libsuews.a ]; then \
		echo ""; \
		echo "=========================================="; \
		echo " ⚠️  SUEWS library not found!"; \
		echo "=========================================="; \
		echo ""; \
		echo "Building SUEWS library first..."; \
		echo ""; \
		$(MAKE) build-lib; \
	fi

# Build SUEWS library
build-lib:
	@echo ""
	@echo "=========================================="
	@echo " Building SUEWS library..."
	@echo "=========================================="
	cd ../SUEWS/src/suews && $(MAKE) -f Makefile.lib clean
	cd ../SUEWS/src/suews && $(MAKE) -f Makefile.lib -j4
	cd ../SUEWS/src/suews && $(MAKE) -f Makefile.lib install
	@echo ""
	@echo "✓ SUEWS library built successfully"
	@echo ""

# Patch configure.wrf after running ./configure
patch:
	@echo ""
	@echo "=========================================="
	@echo " Patching configure.wrf..."
	@echo "=========================================="
	python3 patch_configure.py

# Clean coupling artifacts (not SUEWS library)
clean:
	@echo "Cleaning coupling artifacts..."
	rm -f patch_configure.py
	@echo "Note: To clean SUEWS library, run:"
	@echo "  cd ../SUEWS/src/suews && make -f Makefile.lib clean"

# Clean everything including SUEWS library
distclean: clean
	@echo "Cleaning SUEWS library..."
	cd ../SUEWS/src/suews && $(MAKE) -f Makefile.lib clean

# Show help
help:
	@echo ""
	@echo "WRF-SUEWS Coupling Automator"
	@echo "=============================="
	@echo ""
	@echo "Usage:"
	@echo "  make              - Set up WRF-SUEWS coupling (checks/builds library if needed)"
	@echo "  make build-lib    - Build SUEWS library only"
	@echo "  make patch        - Patch configure.wrf after running ./configure"
	@echo "  make clean        - Clean coupling artifacts"
	@echo "  make distclean    - Clean everything including SUEWS library"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo "Full workflow:"
	@echo "  1. make              # Sets up coupling, builds library if needed"
	@echo "  2. cd ../compilation-YYYYMMDD"
	@echo "  3. ./configure       # Select your platform"
	@echo "  4. cd ../coupling-automator"
	@echo "  5. make patch        # Patch configure.wrf for SUEWS"
	@echo "  6. cd ../compilation-YYYYMMDD"
	@echo "  7. ./compile em_real # Compile WRF-SUEWS"
	@echo ""
