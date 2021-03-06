#!/bin/bash

module load netcdf/4.3.3.1

chmod -w *dump*

cd restart
cp bios_casa_rst_spinup.nc bios_casa_rst.nc
cp pop_bios_ini_spinup.nc pop_bios_ini.nc
cp bios_climate_rst_spinup.nc bios_climate_rst.nc
cp bios_cable_rst_spinup.nc bios_cable_rst.nc

cd ../.

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.1700_1799_casa_luc_only cable.nml
cp driver_files/LUC.nml.1700 LUC.nml

logfile='logs/log_casa_luc_only_1700_1799'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable >& $logfile


cd restart
cp pop_bios_ini.nc pop_bios_ini_casa_luc_only_1700_1799.nc
cp bios_casa_rst.nc bios_casa_rst_casa_luc_only_1700_1799.nc
cp bios_LUC_rst.nc bios_LUC_rst_casa_luc_only_1700_1799.nc


cd ../outputs
mv bios_out_casa.nc bios_out_casa_luc_only_1700_1799.nc
mv bios_out_LUC.nc bios_out_LUC_1700_1799.nc

cd ../.
chmod +w *dump*
