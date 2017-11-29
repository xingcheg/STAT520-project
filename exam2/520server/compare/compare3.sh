#!/bin/bash
#Submit this script with: sbatch thefilename
#SBATCH -t 96:00:00   # walltime
#SBATCH -N 1   # number of nodes in this job
#SBATCH -n 1   # total number of processor cores in this job
#SBATCH --mail-user=xguo@iastate.edu   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --output=job.%J.out #tell it to store the error messages to a file
module load R 

R CMD BATCH compare3.R
