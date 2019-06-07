#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.premet bios.nml
cp driver_files/cable.nml.1860_1899_luc cable.nml
cp driver_files/LUC.nml.1860 LUC.nml

logfile='logs/log_1860_1899_luc'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable>& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_1860_1899_LUC.nc
cp bios_climate_rst.nc bios_climate_rst_1860_1899_LUC.nc
cp bios_casa_rst.nc bios_casa_rst_1860_1899_LUC.nc
cp bios_cable_rst.nc bios_cable_rst_1860_1899_LUC.nc
cp bios_luc_rst.nc bios_luc_rst_1860_1899.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_1860_1899_LUC.nc
mv bios_out_casa.nc bios_out_casa_1860_1899_LUC.nc
mv bios_out_LUC.nc bios_out_LUC_1860_1899.nc

cd ../.
