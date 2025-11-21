# WRF-SUEWS Coupling Optimization: Pre-compiled Library Strategy

**Date:** 2025-11-14
**Status:** Proposed
**Author:** Claude Code Analysis

## Executive Summary

Optimize WRF-SUEWS build times by pre-compiling SUEWS as a static library (`libsuews.a`) instead of merging all source files into a single monolithic Fortran file. This reduces rebuild times by ~50% (from 10-15 min to 5 min).

## Current Approach (Monolithic Merge)

### Architecture
```
SUEWS/*.f95 (73 files)
SPARTACUS/*.F90 (18 files)
         ↓
   gen_suewsdrv.py (merge all)
         ↓
module_sf_suewsdrv.F (61,000 lines, 73 modules)
         ↓
   WRF compilation (10-15 min total)
```

### Problems
1. **Slow compilation**: 61K-line file takes ~60 seconds to compile
2. **Full rebuild required**: Any SUEWS change requires complete WRF rebuild
3. **No incremental builds**: Can't leverage make's dependency tracking
4. **Memory intensive**: Single-file compilation uses significant RAM
5. **Development friction**: Long iteration cycles during coupling development

## Proposed Approach (Pre-compiled Library)

### Architecture
```
┌─────────────────────────────────────────┐
│  Phase 1: Build SUEWS Library (one-time)│
├─────────────────────────────────────────┤
│  SUEWS/src/suews/                       │
│    ├── *.f95 → *.o (incremental)        │
│    ├── ext_lib/spartacus-surface/       │
│    │   └── *.F90 → *.o                  │
│    └── libsuews.a (static archive)      │
│                                          │
│  Install artifacts:                     │
│    ├── lib/libsuews.a                   │
│    └── include/*.mod (module files)     │
└─────────────────────────────────────────┘
            ↓ (static link)
┌─────────────────────────────────────────┐
│  Phase 2: Build WRF-SUEWS              │
├─────────────────────────────────────────┤
│  WRF/phys/                              │
│    ├── module_sf_suews.F (thin wrapper) │
│    └── [link] -lsuews                   │
│                                          │
│  WRF/Registry/                          │
│    └── registry.suews (metadata)        │
└─────────────────────────────────────────┘
```

## Implementation Plan

### 1. SUEWS Library Build System

Create `SUEWS/src/suews/Makefile.lib`:

```makefile
# Compiler settings
FC = gfortran
FCFLAGS = -O2 -fPIC -ffree-form -ffree-line-length-none \
          -fallow-argument-mismatch -fallow-invalid-boz

# Directories
SRCDIR = .
SPARTACUS_BASE = ext_lib/spartacus-surface
BUILDDIR = build
LIBDIR = ../../lib
MODDIR = ../../include

# Source files (in dependency order)
SPARTACUS_UTILS = \
    $(SPARTACUS_BASE)/utilities/parkind1.F90 \
    $(SPARTACUS_BASE)/utilities/print_matrix.F90 \
    $(SPARTACUS_BASE)/utilities/yomhook.F90 \
    $(SPARTACUS_BASE)/utilities/radiation_io.F90 \
    $(SPARTACUS_BASE)/utilities/easy_netcdf.F90

SPARTACUS_RADTOOL = \
    $(SPARTACUS_BASE)/radtool/radiation_constants.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_legendre_gauss.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_matrix.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_schur.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_eigen_decomposition.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_calc_matrices_lw_eig.F90 \
    $(SPARTACUS_BASE)/radtool/radtool_calc_matrices_sw_eig.F90

SPARTACUS_RADSURF = \
    $(SPARTACUS_BASE)/radsurf/radsurf_config.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_canopy_properties.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_boundary_conds_out.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_canopy_flux.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_sw_spectral_properties.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_lw_spectral_properties.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_overlap.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_forest_sw.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_forest_lw.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_urban_sw.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_urban_lw.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_simple_spectrum.F90 \
    $(SPARTACUS_BASE)/radsurf/radsurf_interface.F90

# SUEWS source files (from Makefile parsing)
SUEWS_SRCS = $(shell $(SRCDIR)/parse_makefile.py src/Makefile)

# All sources
ALL_SRCS = $(SPARTACUS_UTILS) $(SPARTACUS_RADTOOL) $(SPARTACUS_RADSURF) $(SUEWS_SRCS)
ALL_OBJS = $(patsubst %.F90,$(BUILDDIR)/%.o,$(patsubst %.f95,$(BUILDDIR)/%.o,$(ALL_SRCS)))

# Targets
.PHONY: all clean install

all: $(LIBDIR)/libsuews.a

$(BUILDDIR)/%.o: %.f95
	@mkdir -p $(dir $@)
	$(FC) $(FCFLAGS) -J$(BUILDDIR) -I$(BUILDDIR) -c $< -o $@

$(BUILDDIR)/%.o: %.F90
	@mkdir -p $(dir $@)
	$(FC) $(FCFLAGS) -J$(BUILDDIR) -I$(BUILDDIR) -c $< -o $@

$(LIBDIR)/libsuews.a: $(ALL_OBJS)
	@mkdir -p $(LIBDIR)
	ar rcs $@ $^
	@echo "Created SUEWS library: $@"

install: $(LIBDIR)/libsuews.a
	@mkdir -p $(MODDIR)
	cp $(BUILDDIR)/*.mod $(MODDIR)/
	@echo "Installed modules to $(MODDIR)/"
	@echo ""
	@echo "SUEWS library ready for WRF coupling!"
	@echo "  Library: $(LIBDIR)/libsuews.a"
	@echo "  Modules: $(MODDIR)/*.mod"

clean:
	rm -rf $(BUILDDIR) $(LIBDIR)/libsuews.a $(MODDIR)/*.mod
```

Helper script `parse_makefile.py`:
```python
#!/usr/bin/env python3
"""Extract SUEWS source files from Makefile in dependency order"""
import sys
import re

def parse_suews_makefile(makefile_path):
    with open(makefile_path) as f:
        content = f.read()

    # Extract dependency rules
    # Format: target.o: dep1.o dep2.o
    deps = {}
    for match in re.finditer(r'(\w+)\.o\s*:\s*([^\n]+)', content):
        target = match.group(1)
        prereqs = match.group(2).strip().split()
        deps[target] = [p.replace('.o', '') for p in prereqs if p.endswith('.o')]

    # Topological sort
    sorted_files = []
    visited = set()

    def visit(node):
        if node in visited:
            return
        visited.add(node)
        for dep in deps.get(node, []):
            visit(dep)
        sorted_files.append(f"src/{node}.f95")

    for target in deps.keys():
        visit(target)

    return sorted_files

if __name__ == '__main__':
    files = parse_suews_makefile(sys.argv[1])
    print(' '.join(files))
```

### 2. Modify Coupling Automator

Update `coupling-automator/automate_main.py`:

```python
# After imports
from pathlib import Path
import subprocess

# Path definitions (existing)
path_src_WRF = Path("../WRF")
path_src_SUEWS = Path("../SUEWS/src/suews")
today = time.strftime("%Y%m%d")
path_working = Path(f"../compilation-{today}")

# NEW: Check for pre-built SUEWS library
path_suews_lib = Path("../SUEWS/lib/libsuews.a")
path_suews_include = Path("../SUEWS/include")

if not path_suews_lib.exists():
    print("=" * 60)
    print("ERROR: SUEWS library not found!")
    print("=" * 60)
    print("\nYou must build the SUEWS library first:")
    print("  cd ../SUEWS/src/suews")
    print("  make -f Makefile.lib")
    print("  make -f Makefile.lib install")
    print()
    sys.exit(1)

if not path_suews_include.exists() or not list(path_suews_include.glob("*.mod")):
    print("ERROR: SUEWS module files not found!")
    print("Run: cd ../SUEWS/src/suews && make -f Makefile.lib install")
    sys.exit(1)

print(f"✓ Found SUEWS library: {path_suews_lib}")
print(f"✓ Found SUEWS modules: {path_suews_include}")

# NEW: wrap all of the above checks in an interactive wizard so nobody
# needs to remember the order manually.
#
# coupling-automator/suews_wizard.py
def main():
    print("WRF-SUEWS setup wizard\n")
    suews_home = Path("../SUEWS")
    if not path_suews_lib.exists():
        if prompt_yes_no("Library missing. Build it now?"):
            run(["make", "-f", "Makefile.lib", "clean"], cwd=suews_home / "src/suews")
            run(["make", "-f", "Makefile.lib", "-j", str(cpu_count())], cwd=suews_home / "src/suews")
            run(["make", "-f", "Makefile.lib", "install"], cwd=suews_home / "src/suews")
    if prompt_yes_no("Run WRF ./configure now?"):
        run(["./configure"], cwd=path_working)
        inject_suews_link_flags(path_working / "configure.wrf")
    if prompt_yes_no("Kick off ./compile em_real ?"):
        run(["./compile", "em_real"], cwd=path_working)

# The wizard gives developers a linear checklist with guard rails:
# - verifies library + modules exist (and builds them if requested)
# - records the detected compiler + paths inside coupling-automator/cache.yml
# - reruns the configure/patch steps every time, so the include/link flags
#   are never forgotten after a fresh ./configure
# - can be re-run with --check to merely validate the environment.
# Provide `make wizard` (alias for `python -m coupling_automator.suews_wizard`)
# so the entry point is memorable.

# ... rest of existing code ...

# REMOVE these lines:
# path_sf_suewsdrv = path_working / "phys" / "module_sf_suewsdrv.F"
# print("calling merge_source to generate module_sf_suewsdrve.F")
# merge_source(path_src_SUEWS, path_sf_suewsdrv)

# KEEP these lines (wrapper and registry still needed):
list_file_to_copy = [
    ("module_sf_suews.F", "phys"),
    ("registry.suews", "Registry"),
    ("namelist.suews", "test/em_real"),
]
for file, dst in list_file_to_copy:
    print("copying " + file + " to " + dst)
    copy(file, path_working / dst)
    file_copied = path_working / dst / file
    print(file_copied, "copied?", file_copied.exists())

# NEW: Update WRF Makefile to source shared link flags
print("\nUpdating phys/Makefile to use wrf_suews.mk...")
makefile_path = path_working / "phys" / "Makefile"
with open(makefile_path, 'r') as f:
    makefile_content = f.read()

# Remove module_sf_suewsdrv.o from dependencies
makefile_content = makefile_content.replace('module_sf_suewsdrv.o', '')

include_line = 'include $(WRF_SRC_ROOT_DIR)/../SUEWS/lib/wrf_suews.mk\n'
if include_line not in makefile_content:
    makefile_content = include_line + makefile_content

with open(makefile_path, 'w') as f:
    f.write(makefile_content)

print("✓ phys/Makefile now tracks shared SUEWS flags")
```

### 3. Shared Link Flags (Single Source of Truth)

Create `SUEWS/lib/wrf_suews.mk` so the include/library paths only live in one
place:

```makefile
# SUEWS/lib/wrf_suews.mk
SUEWS_HOME ?= $(WRF_SRC_ROOT_DIR)/../SUEWS
SUEWS_INC  = -I$(SUEWS_HOME)/include
SUEWS_LIB  = -L$(SUEWS_HOME)/lib -lsuews
SUEWS_CPPFLAGS += $(SUEWS_INC)
SUEWS_LDFLAGS += $(SUEWS_LIB)
```

`module_sf_suews.F` inherits `$(SUEWS_CPPFLAGS)` automatically via the include,
so every build (Mac, Linux, HPC) uses the exact same paths. The coupling
automator copies `wrf_suews.mk` into the working tree (or regenerates it when
paths change) and ensures both Make and configure flows include it.

### 4. Update WRF Configuration (automated)

Instead of instructing people to hand-edit `configure.wrf` every time, inject
the shared include from `automate_main.py` right after `./configure` finishes:

```python
def inject_suews_link_flags(configure_path):
    include_line = 'include $(WRF_SRC_ROOT_DIR)/../SUEWS/lib/wrf_suews.mk\n'
    with open(configure_path, 'r') as fh:
        content = fh.readlines()
    if include_line not in content:
        content.insert(1, include_line)
    content = [line.replace('LIB             =',
                            'LIB             = $(SUEWS_LDFLAGS) ')
               if line.startswith('LIB             =') else line
               for line in content]
    content = [line.replace('INCLUDE_MODULES =',
                            'INCLUDE_MODULES = $(SUEWS_CPPFLAGS) ')
               if line.startswith('INCLUDE_MODULES =') else line
               for line in content]
    with open(configure_path, 'w') as fh:
        fh.writelines(content)
```

`automate_main.py` calls `inject_suews_link_flags` inside the wizard (after it
optionally runs `./configure`). Because the function rewrites the freshly-
generated file every time, we never lose the SUEWS paths when developers rerun
`./configure`. Optional `--check` mode can re-open `configure.wrf` and confirm
the include still exists.

### 5. Update Wrapper Module

Keep `module_sf_suews.F` as-is - it's already a thin wrapper that calls SUEWS subroutines. The key difference is that these subroutines will now be linked from `libsuews.a` instead of being in the same compilation unit.

## Build Workflow

### Initial Setup (guided)
```bash
cd coupling-automator
python -m coupling_automator.suews_wizard --init
```

`--init` runs the full flow: build/install `libsuews.a` if missing, capture
`SUEWS_HOME`, launch `./configure`, patch `configure.wrf`, and (optionally)
start `./compile em_real`. Every prompt defaults to “yes” so new contributors
can just press Enter all the way through.

### Subsequent WRF Rebuilds
```bash
cd coupling-automator
python -m coupling_automator.suews_wizard --relink  # skips library rebuild unless needed
```

### Updating SUEWS Only
```bash
# Edit SUEWS source files
cd SUEWS/src/suews
make -f Makefile.lib -j4  # Incremental rebuild
make -f Makefile.lib install

# Register the new modules & relink (fast!)
cd ../../../../coupling-automator
python -m coupling_automator.suews_wizard --relink
```

## Setup Wizard UX

1. **Pre-flight scan** – detects compilers, NETCDF root, and whether `libsuews.a`
   plus `.mod` files already exist. Results are cached in
   `coupling-automator/.suews-wizard-state.yml`.
2. **Library build (optional)** – offers to rebuild only when timestamps or
   compiler versions changed; otherwise it skips straight to verification.
3. **WRF configure orchestration** – can launch `./configure` interactively for
   the user, then immediately re-open `configure.wrf` to inject the shared
   `wrf_suews.mk` include so no manual edits are ever needed.
4. **Compile and relink** – exposes shortcuts such as `--relink` (calls
   `./compile em_real` only if WRF binaries are stale) and `--check` (just
   validates that include/library paths remain intact).
5. **Troubleshooting hints** – when something fails, the wizard prints the exact
   command it ran plus follow-up steps, mimicking a “wizard page” experience for
   developers who prefer guided flows.

## Benefits

### Time Savings
- **First build**: ~10-15 min (same as current)
- **WRF rebuild**: ~5 min (50% faster - was 10-15 min)
- **SUEWS-only update**: ~2-5 min (incremental, was full rebuild)

### Development Experience
1. **Incremental builds**: Only recompile changed SUEWS files
2. **Faster iteration**: Test coupling changes without full rebuild
3. **Better debugging**: Cleaner separation of SUEWS vs WRF issues
4. **Parallel compilation**: `make -j` works properly with separate object files

### Code Quality
1. **Modular architecture**: Clear interface between SUEWS and WRF
2. **Reusability**: One SUEWS library for multiple WRF builds
3. **Testing**: Can test SUEWS library independently
4. **Version management**: Easier to track which SUEWS version is linked

## Trade-offs

### Disadvantages
1. **Initial complexity**: More build system configuration
2. **Path dependencies**: Library paths must be correctly configured
3. **Module compatibility**: `.mod` files must match library version
4. **Two-phase build**: Must remember to rebuild SUEWS library when needed

### When NOT to Use This Approach
- One-off builds where build time doesn't matter
- SUEWS changes very frequently (more than WRF)
- Build system automation is not available
- Shared library complications on target platform

## Migration Path

### Phase 1: Proof of Concept (1-2 hours)
- Create `Makefile.lib` for SUEWS
- Test library compilation independently
- Verify all symbols are exported

### Phase 2: Integration (2-3 hours)
- Modify `automate_main.py` to use library
- Update WRF Makefiles for linking
- Test full build process

### Phase 3: Validation (1 hour)
- Run WRF-SUEWS test case
- Verify numerical results match current approach
- Benchmark build times

### Phase 4: Documentation (1 hour)
- Update `README.md` with new build instructions
- Document troubleshooting for common issues
- Add build time comparison metrics

**Total Implementation Time: ~6-7 hours**

## Technical Considerations

### Fortran Module Compatibility
- `.mod` files are compiler-specific
- Must use same gfortran version for SUEWS library and WRF
- Module interface changes require library rebuild

### Symbol Visibility
- Fortran library exports all PUBLIC symbols by default
- SUEWS subroutines called from `module_sf_suews.F` must be PUBLIC
- Check with: `nm libsuews.a | grep -i subroutine_name`

### Static vs Shared Library
- Current plan: Static library (`.a`) for portability
- Alternative: Shared library (`.dylib`/`.so`) for smaller executables
- Static linking recommended for HPC portability

### Platform-Specific Notes

#### Apple Silicon (M-series)
- Use `-fPIC` flag for position-independent code
- Library location: `/Users/tingsun/conductor/wrf-suews/SUEWS/lib/`
- Module location: `/Users/tingsun/conductor/wrf-suews/SUEWS/include/`

#### JASMIN HPC
- Use Intel compiler: `module load intel/20.0.0`
- Adjust Makefile.lib for Intel compiler flags
- May need `-fPIC` equivalent for Intel (`-fpic`)

## Testing Strategy

### Unit Test: SUEWS Library Build
```bash
cd SUEWS/src/suews
make -f Makefile.lib clean
make -f Makefile.lib -j4
ls -lh ../../lib/libsuews.a  # Should be ~10-20 MB
ls ../../include/*.mod | wc -l  # Should be ~73 modules
nm ../../lib/libsuews.a | grep -c " T "  # Count exported symbols
```

### Integration Test: WRF Linking
```bash
cd compilation-20251114
./clean -a
./configure
./compile em_real 2>&1 | tee build.log
grep -i "undefined reference" build.log  # Should be empty
ls -lh main/wrf.exe  # Should exist and be ~100+ MB
```

### Validation Test: Numerical Accuracy
```bash
# Run test case with both approaches
# Compare outputs bit-for-bit
diff wrfout_monolithic.nc wrfout_library.nc
# Should be identical
```

## Future Optimizations

### 1. Ccache Integration
```makefile
FC = ccache gfortran
# Caches compilation results for even faster rebuilds
```

### 2. Distributed Builds
```bash
make -j$(nproc) -l$(nproc)
# Parallel compilation across all CPU cores
```

### 3. Pre-built Binaries
- Distribute pre-compiled `libsuews.a` for common platforms
- Users download instead of compiling
- Requires version matching and ABI compatibility

## References

- SUEWS Makefile: `SUEWS/src/suews/Makefile`
- WRF Build System: [WRF Users Guide Chapter 3](https://www2.mmm.ucar.edu/wrf/users/)
- Fortran Module System: ISO/IEC 1539-1:2010
- Static Linking: GNU ar and ranlib documentation

## Appendix: Dependency Graph

```
parkind1 (no deps)
  ↓
print_matrix (uses parkind1)
yomhook (uses parkind1)
radiation_io (uses parkind1, yomhook)
  ↓
easy_netcdf (uses parkind1, radiation_io)
  ↓
radiation_constants (uses parkind1)
  ↓
radtool_* (uses radiation_constants, parkind1)
  ↓
radsurf_* (uses radtool_*, radiation_io, parkind1)
  ↓
SUEWS modules (uses all above)
```

This dependency order is critical for successful compilation.

---

**Status:** Ready for implementation
**Priority:** Medium (quality of life improvement)
**Risk:** Low (can revert to monolithic approach if issues arise)
