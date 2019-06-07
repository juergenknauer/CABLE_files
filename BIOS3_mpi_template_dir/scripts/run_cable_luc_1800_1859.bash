#!/bin/bash

module load netcdf/4.3.3.1

cd restart
cp bios_casa_rst_spinup.nc bios_casa_rst.nc
cp pop_bios_ini_spinup.nc pop_bios_ini.nc
cp bios_climate_rst_spinup.nc bios_climate_rst.nc
cp bios_cable_rst_spinup.nc bios_cable_rst.nc

cd ../.

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.1800_1859_luc cable.nml
cp driver_files/LUC.nml.1800 LUC.nml

logfile='logs/log_1800_1859_luc'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable>& $logfile

cd restart
cp pop_bios_ini.nc pop_bios_ini_1800_1859_LUC.nc
cp bios_climate_rst.nc bios_climate_rst_1800_1859_LUC.nc
cp bios_casa_rst.nc bios_casa_rst_1800_1859_LUC.nc
cp bios_cable_rst.nc bios_cable_rst_1800_1859_LUC.nc
cp bios_LUC_rst.nc bios_LUC_rst_1800_1859.nc

cd ../outputs
mv bios_out_cable.nc bios_out_cable_1800_1859_LUC.nc
mv bios_out_casa.nc bios_out_casa_1800_1859_LUC.nc
mv bios_out_LUC.nc bios_out_LUC_1800_1859.nc

cd ../.
