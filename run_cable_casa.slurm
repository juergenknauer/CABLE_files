#!/bin/bash

#SBATCH --time=1:30:00
#SBATCH --mem=5GB
#SBATCH --job-name="CABLE_site"


module del intel-cc intel-fc
module add intel-cc/16.0.1.150 intel-fc/16.0.1.150
module unload intel-mpi/5.0.1.035
module add netcdf/4.3.3.1 openmpi/1.8.8

python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CumberlandPlain 2013 2018 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Ridgefield 2017 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Warra 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CumberlandPlain 2013 2018 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Ridgefield 2017 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Warra 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CumberlandPlain 2013 2018 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Ridgefield 2017 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Warra 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py AdelaideRiver 2008 2008 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py CumberlandPlain 2013 2018 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Ridgefield 2017 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
python ./run_cable_site_CNP_meta.py Warra 2014 2017 FALSE /OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site /OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux /OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R FALSE imp
