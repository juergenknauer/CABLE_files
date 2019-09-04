#!/usr/bin/env Rscript
# Juergen Knauer
# May 2019
# create observations.csv files for use in PEST


source("../plots_R/CABLE_plots_functions.R")   # assumed to be in the sister folder! (R packages loaded in there as well)
missval <- -9999.0


# Set command line parameters
Args <- commandArgs(trailingOnly=TRUE)
if(length(Args) != 3) {
  message("create_observations_csv.R requires the following command line arguments: 1) site, 2) obs_dir (OzFLUX nc files), 3) out_dir (csv files)");quit()
} 

site     <- as.character(Args[1])
obs_dir  <- as.character(Args[2])
out_dir  <- as.character(Args[3])


# load observation files and variables
obs <- nc_open(paste0(obs_dir,"/",site,"_L6.nc"))



GPP  <- round(ncvar_get(obs,varid='GPP_SOLO'),6)
NEP  <- round(ncvar_get(obs,varid='NEP_SOLO'),6)
evap <- round(ncvar_get(obs,varid='ET'),6)   # mm
Reco <- round(ncvar_get(obs,varid='ER_SOLO'),6)

GPP_QC  <- ncvar_get(obs,varid='GPP_SOLO_QCFlag')
NEP_QC  <- ncvar_get(obs,varid='NEP_SOLO_QCFlag')
evap_QC <- ncvar_get(obs,varid='ET_QCFlag')
Reco_QC <- ncvar_get(obs,varid='ER_SOLO_QCFlag')


# filter variables for QCFlags!
# note that at the moment, missing QCFlags are not filtered out!
GPP[GPP_QC > 0]   <- missval
NEP[NEP_QC > 0]   <- missval
evap[evap_QC > 0] <- missval
Reco[Reco_QC > 0] <- missval

  

# extract time information
time_obs <- obs$dim$time$vals
dmy      <- dmy.obs(time_obs,origin='1800-01-01')
timestep <- ifelse((time_obs[2] - time_obs[1]) * secperday > 1801,"Hourly","HalfHourly") 
dmy[,'hod'] <- floor(dmy[,'hod'])  
  

# create data.frame and write to file
csv_vars <- data.frame(dmy[,'year'],dmy[,'month'],dmy[,'day'],dmy[,'hod'],dmy[,'minute'],
                       GPP,NEP,evap,Reco)
colnames(csv_vars) <- c('Year','Month','Day','Hour','Minute','GPP','NEP','evap','Reco')


Sys.umask("037")  # should be chmod 777
write.table(csv_vars,file=paste0(out_dir,"/",site,"_",timestep,"_SOLO.csv"),sep=",",col.names=TRUE,row.names=FALSE,quote=FALSE)

