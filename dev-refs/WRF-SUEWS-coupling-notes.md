# WRF–SUEWS Coupling Story (No Glory, Just Details)

- **Workspace:** `compilation-20251120` (cloned from `WRF` by the coupling automator).
- **Toolchain locked:** `/opt/homebrew/bin/gfortran`, `/opt/homebrew/bin/gcc-15`, `/opt/homebrew/bin/mpif90/mpicc`; NETCDF at `/opt/homebrew`.
- **SUEWS linkage:** `configure.wrf` now includes `../SUEWS/lib/wrf_suews.mk`, appends `$(SUEWS_CPPFLAGS)` to `INCLUDE_MODULES`, and `$(SUEWS_LDFLAGS)` to `LIB`.

## Sequence

1. **Reset and reconfigure**
   - Ran `./configure` with choice 15 (gnu serial) and nesting=1 under `NETCDF=/opt/homebrew`.
   - Re-patched `configure.wrf` for Homebrew compilers + SUEWS flags.
   - Re-enabled `phys/physics_mmm` externals (ran `tools/manage_externals/checkout_externals`).

2. **Makefile surgery (physics)**
   - `phys/Makefile`: restored `PHYSMMM_MODULES` list and external checkout, removed obsolete `module_sf_suewsdrv` dependency.
   - `main/depend.common`: dropped stale `module_sf_suewsdrv.o` requirement.

3. **SUEWS driver alignment**
   - `phys/module_sf_suews.F`: matched the current `SuMin` interface.
     - Added BaseT_HC/soilstore/SnowWater locals; mapped BaseTHDD→BaseT_HC; soilmoist→soilstore; MeltWaterStore→SnowWater.
     - Set znt/ust outputs explicitly after `SuMin` call (znt from `z0m_in_id`, ust=0).
     - Cleaned the call signature ordering/comments to mirror `SuMin_Module`.

4. **MYNN init fix**
   - `phys/module_physics_init.F`: removed misplaced SUEWS arguments to `mynnedmf_init` that broke types/ordering.

5. **Build loops**
   - Multiple `./clean` + `./compile em_real` cycles to chase mod/link errors.
   - Final compile succeeded; executables produced: `compilation-20251120/main/{wrf.exe,real.exe,tc.exe,ndown.exe}`.
   - Build log: `compilation-20251120/log.compile`.

## Artifacts/Checks

- `compilation-20251120/phys/module_sf_suews.mod` present (driver builds).
- `compilation-20251120/phys/physics_mmm/` populated from NCAR/MMM-physics external.
- `log.compile` shows no `Fatal Error` entries in the final run.

## Next steps (your side)

- Run `test/em_real` (or your case) with the new binaries to validate runtime behavior.
- Consider pinning `configure.wrf` changes into `arch/configure.defaults` if you want reconfigures to stick. 

## Timeline (GMT)

- 16:07 – Environment check; `configure.wrf` absent post-clean.
- 16:09–16:12 – Reconfigured (`./configure` opt 15, nesting=1, `NETCDF=/opt/homebrew`); repatched `configure.wrf` for Homebrew + SUEWS flags; re-enabled `physics_mmm` checkout.
- 16:16–16:22 – First rebuild pass; failures on missing modules/SUEWS mismatch.
- 16:23–16:45 – Iterative fixes: restored `PHYSMMM_MODULES`, dropped `module_sf_suewsdrv`, aligned `module_sf_suews` call, fixed MYNN init call.
- 16:50–17:10 – Clean/compile loops; SUEWS interface and physics mods stabilize (`module_sf_suews` builds).
- 17:20–18:50 – Final long `./compile em_real`; succeeded, produced `wrf.exe`, `real.exe`, `tc.exe`, `ndown.exe` in `main/`; `log.compile` clean of fatal errors.

## Runtime validation (em_quarter_ss with SUEWS)

- 19:45 – Saved the original 30 min wrfout as `wrfout_d01_0001-01-01_00:00:00_short`.
- 19:45–19:46 – Extended `namelist.input` to 3 h (10 min history), reran `ideal.exe` then `wrf.exe`; SUEWS init ok, final wrfout timestamp `2025-11-20 19:46:11 UTC`.
- 19:51 – Rebuilt `notebooks/suews_quarter_demo.ipynb` to visualize SUEWS fluxes/states and spatial patterns from the new run.
- 20:05 – Added a landuse fallback in `suewsinit` (fills empty `LANDUSEF` with cat 1) and recompiled (`./compile em_quarter_ss`).
- 20:15–20:25 – Tried a daytime rerun (start 12 UTC, rad schemes on, sfclay/pbl enabled) to drive SUEWS; runs now abort with `fatal error in SUEWS: Check input file SUEWS_Conductance.txt.` even after linking the sample WSPS/Swindon inputs. Needs a consistent SUEWS input suite (e.g., SiteSelect/Conductance tables matching the case) before fluxes will populate.

Artifacts:

- `compilation-20251120/test/em_quarter_ss/wrfout_d01_0001-01-01_00:00:00` (3 h, 19 frames) — SUEWS fields are zero because the driver lacked complete input tables. Backup of the short test: `..._00:00:00_short`.
- `notebooks/suews_quarter_demo.ipynb` – wired to read `wrfout_d01_0001-01-01_00:00:00`; update the path once a valid SUEWS run completes.
