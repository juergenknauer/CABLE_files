#!/bin/bash

#SBATCH --time=2:00:00
#SBATCH --mem=20GB
#SBATCH --job-name="PEST_site"
#SBATCH --ntasks-per-node=9
#SBATCH --error=CumbPest.err
#SBATCH --output=CumbPest.out

module del intel-cc intel-fc
module add intel-cc/16.0.1.150 intel-fc/16.0.1.150
module unload intel-mpi/5.0.1.035
module add netcdf/4.3.3.1 openmpi/1.8.8-ofed42
module load  pest/12.2.0-beo-test
ulimit -s 10240

WD=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site/CumberlandPlain

rm -rf ./tmp/PEST/PEST1/CABLE*
mpirun ppest ${WD}/PEST_case /m ${WD}/tmp/PEST/PEST1/CABLE > log_PEST_case
