#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.standard bios.nml
cp driver_files/cable.nml.1900_1989 cable.nml
cp driver_files/LUC.nml LUC.nml

logfile='logs/log_1900_1989'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable>& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_1900_1989.nc
cp bios_climate_rst.nc bios_climate_rst_1900_1989.nc
cp bios_casa_rst.nc bios_casa_rst_1900_1989.nc
cp bios_cable_rst.nc bios_cable_rst_1900_1989.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_1900_1989.nc
mv bios_out_casa.nc bios_out_casa_1900_1989.nc

cd ../.
