#!/bin/bash
# sb-run_wrf.sh

#SBATCH -p par-multi
#SBATCH -n 36
#SBATCH -o %j.log
#SBATCH -e %j.err
#SBATCH --time=48:00

echo "Running WRF"
mpirun ./wrf.exe