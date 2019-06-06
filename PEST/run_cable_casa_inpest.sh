#!/bin/bash


rm -f veg_params_pest.txt
rm -f pftlookup_pest.csv

bdir=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site/CumberlandPlain
wdir=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site/CumberlandPlain/PEST
# Create def_veg_params and pftlookup files with current parameters
par2par p2p.dat

#cp /flush1/projects/oa_carbon/BIOS3/mdf/Cumberland/ObsOperatorParams.txt .  
ln -s $bdir/cable
ln -s $bdir/params/def_soil_params.txt
ln -s $bdir/params
ln -s $bdir/namelists
ln -s $bdir/met
ln -s $bdir/CABLE-AUX
mkdir outputs
mkdir logs
mkdir restart_files
$wdir/reset_restart_pest.bash  # copy restart files if just running simulation not spin
$wdir/run_cable_site_CNP_meta.py 


# Extract observables (model equivalents of observations) from model output
cp $wdir/ObsSpecs.txt .
cp $wdir/ExtractObservables.nml .
$wdir/ExtractObservables.exe > logs/ExtractObservablesLog.txt

