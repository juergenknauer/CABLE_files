#!/bin/bash

module load netcdf/4.3.3.1


cd restart
cp bios_casa_rst_spin_casa.nc bios_casa_rst.nc
cp pop_bios_ini_spin_casa.nc pop_bios_ini.nc

cd ../.

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.1860_1989_casa_only cable.nml
cp driver_files/LUC.nml LUC.nml

logfile='logs/log_casa_only_1860_1989'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable >& $logfile


cd restart
cp pop_bios_ini.nc pop_bios_ini_casa_only_1860_1989.nc
cp bios_casa_rst.nc bios_casa_rst_casa_only_1860_1989.nc


cd ../outputs
mv bios_out_casa.nc bios_out_casa_casa_only_1860_1989.nc

cd ../.
