#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.from_zero cable.nml
cp driver_files/LUC.nml LUC.nml

logfile='logs/log_zero'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable >& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_zero.nc
cp bios_climate_rst.nc bios_climate_rst_zero.nc
cp bios_casa_rst.nc bios_casa_rst_zero.nc
cp bios_cable_rst.nc bios_cable_rst_zero.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_zero.nc
mv bios_out_casa.nc bios_out_casa_zero.nc
mv bios_out_pop.nc bios_out_pop_zero.nc

cd ../.
