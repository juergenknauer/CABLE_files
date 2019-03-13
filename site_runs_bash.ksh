#!/bin/ksh

# This script runs CABLE at OzFLUX/FLUXNET sites
# Juergen Knauer, February 2019
#

# Global settings:
SITE_LIST=OzFLUX_sitelist_v1.txt
#SITE_LIST=test.txt
BASE_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/CABLE_files
SITE_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site
CODE_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/CABLE_code/NESP_OzFLUX
FORCING_DIR=/OSM/CBR/OA_GLOBALCABLE/work/BIOS3_forcing/site_met

LAI_feedback=FALSE



# site names and number of sites
sites=$(cut -f 1 $SITE_LIST)
let nr_sites=$(wc -l $SITE_LIST | awk '{print $1}')-1 

# clean up slurm file
sed -i '34,$d' run_cable_casa.slurm


for site in $sites; do

    echo starting site $site
    
    # create working directory if not existing (copy from template directory)
    WD=${SITE_DIR}/${site}
    if [ ! -d $WD ]; then
	mkdir $WD

        cd $WD

	# copy files and create links
	ln -s ${CODE_DIR}/offline/cable cable
	cp ${BASE_DIR}/reset_restart.bash .
        cp ${BASE_DIR}/run_cable_site_CNP.py .
	cp ${BASE_DIR}/run_cable_site_CNP_meta.py .
	cp -r ${BASE_DIR}/params .
	cp -r ${BASE_DIR}/namelists .
	chmod -R 755 *  # permissions
	ln -s $FORCING_DIR met
	ln -s /data/hav014/CABLE-AUX  # to be generalized...
      
    fi


    cd $BASE_DIR
    
    ## extract start- and endyear from sitelist (alternative: from metfile)
    startyear=`awk -v site=${site} '$0~site {print $2}' $SITE_LIST`
    endyear=`awk -v site=${site} '$0~site {print $3}' $SITE_LIST`

    ## run on terminal:
    # python run_cable_site_CNP_meta.py $site $startyear $endyear

    ## run on cluster (add to .slurm file):
    echo python ./run_cable_site_CNP_meta.py $site $startyear $endyear $LAI_feedback >> run_cable_casa.slurm
        
done

## send to cluster
sbatch --array=1-${nr_sites} run_cable_casa.slurm


## command line arguments to 'run_cable_site_CNP_meta.py'
# 1: site name
# 2: start year
# 3: end year
# 4: lai_feedback (TRUE or FALSE)

