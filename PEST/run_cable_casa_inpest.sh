#!/bin/bash


rm -f def_veg_params_pest.txt
rm -f pftlookup_pest.csv

wdir=/OSM/CBR/OA_GLOBALCABLE/work/Juergen/single_site/CumberlandPlain
# Create def_veg_params and pftlookup files with current parameters
par2par p2p.dat

#cp /flush1/projects/oa_carbon/BIOS3/mdf/Cumberland/ObsOperatorParams.txt .  
ln -s $wdir/cable
ln -s $wdir/params/def_soil_params.txt
ln -s $wdir/params
ln -s $wdir/namelists
ln -s $wdir/met
ln -s $wdir/CABLE-AUX
mkdir outputs
mkdir logs
mkdir restart_files
$wdir/reset_restart_pest.bash  # copy restart files if just running simulation not spin
$wdir/run_cable_site_CNP.py


# Extract observables (model equivalents of observations) from model output
cp $wdir/ObsSpecs.txt .
cp $wdir/ExtractObservables.nml .
$wdir/ExtractObservables.exe > logs/ExtractObservablesLog.txt

