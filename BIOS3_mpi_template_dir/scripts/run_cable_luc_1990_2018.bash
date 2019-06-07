#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.standard bios.nml
cp driver_files/cable.nml.1990_2018_luc cable.nml
cp driver_files/LUC.nml.1990 LUC.nml

cp bios_cable_rst_1860_1899_LUC.nc bios_cable_rst.nc
cp bios_LUC_rst_1900_1989.nc bios_LUC_rst.nc
cp bios_climate_rst_1900_1989_LUC.nc bios_climate_rst.nc
cp pop_bios_ini_1900_1989_LUC.nc pop_bios_ini.nc
cp bios_casa_rst_1900_1989_LUC.nc bios_casa_rst.nc

logfile='logs/log_1990_2018_luc'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable>& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_1990_2018_LUC.nc
cp bios_climate_rst.nc bios_climate_rst_1990_2018_LUC.nc
cp bios_casa_rst.nc bios_casa_rst_1990_2018_LUC.nc
cp bios_cable_rst.nc bios_cable_rst_1990_2018_LUC.nc
cp bios_LUC_rst.nc bios_LUC_rst_1990_2018.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_1990_2018_LUC.nc
mv bios_out_casa.nc bios_out_casa_1990_2018_LUC.nc
mv bios_out_LUC.nc bios_out_LUC_1990_2018.nc

cd ../.
