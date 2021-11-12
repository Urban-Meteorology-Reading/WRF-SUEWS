#!/bin/bash
# sb-run_wrf.sh

#SBATCH -p par-multi
#SBATCH -n 48
#SBATCH -o %j.log
#SBATCH -e %j.err
#SBATCH --time=48:00:00

# load MPI lib
# https://help.jasmin.ac.uk/article/4896-how-to-submit-an-mpi-parallel-job-to-slurm
# module load eb/OpenMPI/intel/3.1.1
# the module above seems problematic
# use these instead:
module load intel/19.0.0
module load intel/mpi/5.0.1.035

echo "Running WRF"
mpirun ./wrf.exe