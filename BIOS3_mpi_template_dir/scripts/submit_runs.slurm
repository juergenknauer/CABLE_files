#!/bin/bash

#SBATCH --time=5:30:00

#SBATCH --mem=80gb

#SBATCH --job-name="allobs"

#SBATCH --ntasks-per-node=20

#SBATCH --error=allobs.err

#SBATCH --output=allobs.out

module del intel-cc intel-fc
module add intel-cc/16.0.1.150 intel-fc/16.0.1.150
module unload intel-mpi/5.0.1.035 
module add netcdf/4.3.3.1 openmpi/1.8.8


## uncomment the lines below to do initialisation with 'limit_labile' set to 'TRUE' (nutrient limitation turned off)
#../scripts/run_cable_init_0.bash
#../scripts/run_cable_spinup_limit_labile.bash
#../scripts/run_cable_spin_casa_limit_labile.bash
#../scripts/run_cable_spinup_limit_labile.bash
#../scripts/run_cable_spin_casa_limit_labile.bash
#../scripts/run_cable_spinup_limit_labile.bash
#../scripts/run_cable_spin_casa_limit_labile.bash

## uncomment the lines below to continue initialisation with 'limit_labile' set to 'TRUE' (nutrient limitation turned off)

#../scripts/run_cable_spinup.bash
#../scripts/run_cable_spin_casa.bash
#../scripts/run_cable_spinup.bash
#../scripts/run_cable_spin_casa.bash
#../scripts/run_cable_spinup.bash


## uncomment lines below for transient run
#../scripts/run_cable_1860_1899.bash  # this one uses recycled met
#../scripts/run_cable_1900_1989.bash
../scripts/run_cable_1990_2018.bash

## uncomment lines below for CASA-ONLY run (eg to estimate stocks after changing biogeochemical params)
#../scripts/run_cable_spin_casa.bash
#../scripts/run_casa_only_1860_1989.bash
#../scripts/run_casa_only_1990_2017.bash
