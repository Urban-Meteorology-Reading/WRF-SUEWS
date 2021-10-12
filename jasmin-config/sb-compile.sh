#!/bin/bash
# sb-compile.sh
#SBATCH --partition=short-serial
#SBATCH -o %j.out
#SBATCH -e %j.err
#SBATCH --time=02:30

./compile em_real >& log.compile