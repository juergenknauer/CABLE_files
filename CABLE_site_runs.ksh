#!/bin/ksh

# This script runs CABLE at multiple OzFLUX/FLUXNET sites
# Juergen Knauer, February/March 2019
#

# Global settings:
#SITE_LIST=OzFLUX_sitelist_v1.txt
SITE_LIST=test_sites.txt
SITE_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site
AUX_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Vanessa/CABLE-AUX
LOG_DIR=${SITE_DIR}/logs
CODE_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/CABLE_code/gm_testing
FORCING_DIR=/OSM/CBR/OA_GLOBALCABLE/work/BIOS3_forcing/site_met
OBS_DIR=/OSM/CBR/OA_GLOBALCABLE/work/Data_EC/OzFlux
PLOT_DIR=/OSM/CBR/OA_GLOBALCABLE/work/CABLE_files/plots_R

EXP_NAME=imp
LAI_feedback=FALSE
finite_gm=FALSE



### no changes needed beyond that point ###

# site names and number of sites
sites=$(cut -f 1 $SITE_LIST)
let nr_sites=$(wc -l $SITE_LIST | awk '{print $1}')-1 

# clean up slurm file
sed -i '29,$d' run_cable_casa.slurm

# set current directory as base directory
BASE_DIR=$PWD

# define (and create) log folder
if [ ! -d $LOG_DIR ]; then
    mkdir $LOG_DIR
fi



for site in $sites; do

    echo starting site $site
    
    # create working directory if not existing (copy from template directory)
    WD=${SITE_DIR}/${site}
    if [ ! -d $WD ]; then
	mkdir $WD
        cd $WD

	# create links
	ln -s $FORCING_DIR met
	ln -s $AUX_DIR
	chmod -R 755 *  # permissions
    fi

    cd $WD

    cp -r ${BASE_DIR}/params .
    cp -r ${BASE_DIR}/namelists .
    rm cable
    ln -s ${CODE_DIR}/offline/cable cable
    chmod 775 cable
    cp ${BASE_DIR}/reset_restart.bash .
    cp ${BASE_DIR}/run_cable_site_CNP.py .
    cp ${BASE_DIR}/run_cable_site_CNP_meta.py .


    
    cd $BASE_DIR
    
    ## extract start- and endyear from sitelist (alternative: from metfile)
    startyear=`awk -v site=${site} '$0~site {print $2}' $SITE_LIST`
    endyear=`awk -v site=${site} '$0~site {print $3}' $SITE_LIST`

    ## run on terminal:
    # python run_cable_site_CNP_meta.py $site $startyear $endyear

    ## run on cluster (add to .slurm file):
    echo python ./run_cable_site_CNP_meta.py $site $startyear $endyear $LAI_feedback \
	 $SITE_DIR $OBS_DIR $PLOT_DIR $finite_gm $EXP_NAME >> run_cable_casa.slurm
        
done

## send to cluster
sbatch --array=1-${nr_sites} --output=${LOG_DIR}/%x_%a.out --error=${LOG_DIR}/%x_%a.err run_cable_casa.slurm


## command line arguments to 'run_cable_site_CNP_meta.py'
# 1: site name
# 2: start year
# 3: end year
# 4: lai_feedback? (TRUE or FALSE)
# 5: site directory
# 6: observations (EC data) directory
# 7: plot directory (location of plotting scripts)
# 8: finite gm? (TRUE or FALSE)
# 9: experiment name

