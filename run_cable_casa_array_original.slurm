#!/bin/bash
#SBATCH --time=6:00:00
#SBATCH --mem=5GB
#SBATCH --job-name=CABLE_site

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


read site startyear endyear < <(sed -n $SLURM_ARRAY_TASK_ID\p <<EOD
AdelaideRiver 2008 2008 
CumberlandPlain 2013 2018 
Ridgefield 2017 2017 
Warra 2014 2017
EOD
)

lai_feedback=FALSE
site_dir=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site
obs_dir=/OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux
plot_dir=/OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R 
finite_gm=FALSE 
exp_name=imp

python ./run_cable_site_CNP_meta.py $site $startyear $endyear $lai_feedback $site_dir $obs_dir $plot_dir $finite_gm $exp_name



