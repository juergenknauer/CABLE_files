#!/bin/bash
#SBATCH --job-name=PESTCumb
#SBATCH --time=20:00:00
#SBATCH --nodes=6
#SBATCH --ntasks-per-node=1
#SBATCH --mem=2GB

module del intel-cc intel-fc
module add intel-cc/16.0.1.150 intel-fc/16.0.1.150
module unload intel-mpi/5.0.1.035
module add netcdf/4.3.3.1 openmpi/1.8.8

#module load intel-mkl
module load pest/12.2.0-beo-test
ulimit -s 10240
cd $SLURM_SUBMIT_DIR
rm -rf $FLUSHDIR/PEST/PEST1/CABLE*
mpirun -np 6 ppest /flush1/projects/oa_carbon/BIOS3/mdf/Cumberland/PEST_Cumb /m $FLUSHDIR/PEST/PEST1/CABLE > logCumbPEST
