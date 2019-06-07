#!/bin/bash

module load netcdf/4.3.3.1

cp driver_files/bios.nml.spinup bios.nml
cp driver_files/cable.nml.spin_casa_limit_labile cable.nml
cp driver_files/LUC.nml LUC.nml

logfile='logs/log_spin_casa_limit_labile'
mpirun -np 20 ./cable-mpi >& $logfile
#./cable >& $logfile


cd restart
cp pop_bios_ini.nc pop_bios_ini_spin_casa_limit_labile.nc
cp bios_casa_rst.nc bios_casa_rst_spin_casa_limit_labile.nc


cd ../outputs
mv bios_out_casa.nc bios_out_casa_spin_casa_limit_labile.nc

cd ../.
