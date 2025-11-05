# Quick Start Guide: Testing WRF-SUEWS Updates

This guide helps you quickly verify the WRF v4.7.1 and SUEWS 2025.10.15 coupling updates.

## TL;DR - Run This First

```bash
cd /Users/tingsun/conductor/wrf-suews/.conductor/austin
./test_coupling.sh
```

This runs all verification checks and tells you if everything is ready.

---

## What Changed

**Version Updates:**
- WRF: v4.0.2 → **v4.7.1**
- SUEWS: → **2025.10.15** (from UMEP-dev)

**Coupling Updates:**
- Updated for new SUEWS structure (`src/suews`)
- Fixed version module generation
- Compatible with both new versions

---

## 5-Minute Verification

### 1. Check Submodules

```bash
cd WRF && git describe --tags
# Expected: v4.7.1

cd ../SUEWS && git describe --tags
# Expected: 2025.10.15
```

### 2. Test Coupling Generation

```bash
cd coupling-automator
make  # Runs python3 automate_main.py

# Check success
ls ../compilation-$(date +%Y%m%d)/phys/module_sf_suews*.F
# Expected: Two files listed
```

### 3. Verify Files

```bash
# Check generated driver size
wc -l compilation-*/phys/module_sf_suewsdrv.F
# Expected: ~50,000 lines

# Check SUEWS scheme registered
grep "suewsscheme" compilation-*/Registry/Registry.EM_COMMON
# Expected: Long line with 80+ SUEWS variables
```

---

## Full Compilation Test (15-45 minutes)

### Prerequisites

```bash
# Install if needed
brew install gcc netcdf-c netcdf-fortran

# Set environment
export NETCDF=/opt/homebrew/

# Check
gfortran --version  # Should be GCC 13.x or 14.x
nf-config --version # Should show NetCDF version
```

### Compile

```bash
cd compilation-$(date +%Y%m%d)

# Configure
./configure
# Choose: 15 (serial gcc/gfortran)
# Choose: 1 (basic nesting)

# Fix for Apple Silicon
sed -i.bak 's/FCBASEOPTS.*=.*$(FCBASEOPTS_NO_G) $(FCDEBUG)$/& -fallow-invalid-boz -fallow-argument-mismatch/' configure.wrf

# Compile (takes 15-45 minutes)
./compile em_real >& log.compile

# Check success
ls -lh main/wrf.exe main/real.exe
# Expected: Both executables exist
```

### Monitor Compilation

In another terminal:
```bash
tail -f log.compile
```

Watch for:
- Registry processing (early stage)
- Module compilation (middle stage)
- Linking (final stage)
- "SUCCESS" message at end

---

## Verify Coupling Worked

### Check Generated Files

```bash
# SUEWS modules should be compiled
grep "module_sf_suews" log.compile | grep -i compile
# Expected: module_sf_suews.o and module_sf_suewsdrv.o

# Check no errors
grep -i "error" log.compile | grep -v warning
# Expected: Empty (no errors)
```

### Check SUEWS in Executable

```bash
# SUEWS scheme should be in binary
strings main/wrf.exe | grep -i "suews" | head -5
# Expected: Some SUEWS-related strings

# Symbol check
nm main/wrf.exe | grep -i "suewsdrv" | wc -l
# Expected: > 0
```

---

## Success Indicators

✅ **Coupling Generation:**
- `module_sf_suewsdrv.F` is ~50,000 lines
- Registry has SUEWS scheme registered
- All 7 WRF files modified

✅ **Compilation:**
- `wrf.exe` and `real.exe` exist
- No errors in `log.compile`
- SUEWS modules compiled

✅ **Runtime Ready:**
- Can set `sf_surface_physics = 9` in namelist
- SUEWS-specific variables available
- Can use WSPS to prepare inputs

---

## Quick Troubleshooting

**Problem: "NETCDF not set"**
```bash
export NETCDF=/opt/homebrew/
# Add to ~/.zshrc to persist
```

**Problem: "invalid BOZ constant"**
```bash
# Make sure configure.wrf has:
grep "fallow-invalid-boz" configure.wrf
# If not, re-run the sed command above
```

**Problem: "module_sf_suewsdrv.F too small"**
```bash
# Re-run coupling automator
cd coupling-automator && make
# Check SUEWS source exists:
ls ../SUEWS/src/suews/src/*.f95 | wc -l
# Expected: 30+ files
```

**Problem: "Python import error"**
```bash
# Check numpy/pandas versions
python3 -c "import numpy, pandas; print('OK')"
# If fails: pip3 install numpy pandas
```

---

## What to Report if Tests Fail

Run this and share output:
```bash
./test_coupling.sh > test_output.txt 2>&1
cat test_output.txt
```

Also useful:
```bash
# Last 100 lines of compile log
tail -100 log.compile

# Environment info
gfortran --version
nf-config --version
uname -a
```

---

## Next Steps After Successful Compilation

1. **Set up WSPS** (pre-processor for SUEWS inputs)
   ```bash
   cd WSPS
   conda env create -f environment.yml
   conda activate WSPS
   ```

2. **Prepare test case** (see WSPS/README.md)
   - Get WRF input files from WPS
   - Configure `namelist.suews`
   - Run `python wsps.py`

3. **Run WRF-SUEWS**
   - Set `sf_surface_physics = 9` in namelist.input
   - Copy modified wrfinput files
   - Run real.exe then wrf.exe

---

## Documentation

- **Full Testing Guide**: `TESTING.md` (comprehensive procedures)
- **Main README**: `../README.md` (project overview)
- **CLAUDE.md**: Detailed project context
- **WSPS Guide**: `WSPS/README.md` (pre-processing)

---

## Testing Levels

| Level | Time | Command | What It Tests |
|-------|------|---------|---------------|
| Quick | 2 min | `./test_coupling.sh` | File generation |
| Medium | 5 min | `make` in coupling-automator | Code generation |
| Full | 30 min | `./compile em_real` | Complete build |

---

## Expected Results

**test_coupling.sh output:**
```
✅ All checks passed!
Status: Ready for compilation
```

**After compilation:**
```
ls -lh main/*.exe
-rwxr-xr-x  1 user  staff   45M Oct 28 10:00 wrf.exe
-rwxr-xr-x  1 user  staff   35M Oct 28 10:00 real.exe
```

**Compilation time:**
- Apple M1/M2: ~15-25 minutes
- Apple M3: ~10-15 minutes
- Depends on: CPU cores, memory, SSD speed

---

## Status Check Commands

```bash
# Quick status
./test_coupling.sh | grep "✅\|❌"

# Compilation progress
tail log.compile | grep -E "^=|SUCCESS|ERROR"

# File sizes
du -sh compilation-*/phys/module_sf_suews*.F

# Module count
grep -c "^MODULE " compilation-*/phys/module_sf_suewsdrv.F
```

---

## All Good?

If `./test_coupling.sh` shows all ✅, you're ready to compile!

If compilation succeeds, you have working WRF v4.7.1 + SUEWS 2025.10.15! 🎉
