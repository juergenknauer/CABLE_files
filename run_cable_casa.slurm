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
python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py AliceSpringsMulga 2011 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Calperum 2011 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CapeTribulation 2010 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CowBay 2010 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CumberlandPlain 2013 2018 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py DalyPasture 2008 2012 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py DalyUncleared 2008 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py DryRiver 2009 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Emerald 2012 2013 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py FoggDam 2007 2007 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Gingin 2012 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py GreatWesternWoodlands 2013 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py HowardSprings 2003 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Litchfield 2016 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Otway 2008 2010 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py RedDirtMelonFarm 2012 2012 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Ridgefield 2017 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py RiggsCreek 2011 2016 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py RobsonCreek 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Samford 2011 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py SturtPlains 2009 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py TiTreeEast 2013 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Tumbarumba 2002 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Warra 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Whroo 2012 2016 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py WombatStateForest 2011 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Yanco 2013 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
