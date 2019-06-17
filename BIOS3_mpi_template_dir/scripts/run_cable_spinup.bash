#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.spinup cable.nml
cp driver_files/LUC.nml LUC.nml

logfile='logs/log_spinup'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable>& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_spinup.nc
cp bios_climate_rst.nc bios_climate_rst_spinup.nc
cp bios_casa_rst.nc bios_casa_rst_spinup.nc
cp bios_cable_rst.nc bios_cable_rst_spinup.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_spinup.nc
mv bios_out_casa.nc bios_out_casa_spinup.nc

cd ../.
