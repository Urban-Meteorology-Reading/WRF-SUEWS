#!/bin/bash
# sb-run_wrf.sh

#SBATCH -p par-multi
#SBATCH -n 48
#SBATCH -o %j.log
#SBATCH -e %j.err
#SBATCH --time=48:00

# load MPI lib
# https://help.jasmin.ac.uk/article/4896-how-to-submit-an-mpi-parallel-job-to-slurm
module load eb/OpenMPI/intel/3.1.1

echo "Running WRF"
mpirun ./wrf.exe