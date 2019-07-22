#!/bin/bash

#SBATCH --time=6:00:00

#SBATCH --mem=75GB

#SBATCH --job-name="CABLE_site"

#SBATCH --ntasks-per-node=1

#SBATCH --nodes=1

module del intel-cc intel-fc
module add intel-cc/16.0.1.150 intel-fc/16.0.1.150
module unload intel-mpi/5.0.1.035
module add netcdf/4.3.3.1 openmpi/1.8.8


#./scripts/run_cable_init.bash
#./scripts/run_cable_spinup.bash
#./scripts/run_cable_spin_casa.bash
#./scripts/run_cable_spinup.bash
#./scripts/run_cable_spin_casa.bash
#./scripts/run_cable_spinup.bash
#./scripts/run_cable_spin_casa.bash
#./scripts/run_cable_spinup.bash
#./scripts/run_cable_premet.bash
#./scripts/run_cable_1900_1989.bash
python ./run_cable_site_CNP_meta.py SturtPlains 2009 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
