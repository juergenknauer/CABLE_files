#!/usr/bin/env Rscript
# Juergen Knauer
# April 2019

#---------------------------------------------------------------------------------------------------
rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows   

library(ncdf4)
library(lubridate)



# Set command line parameters
Args <- commandArgs(trailingOnly=TRUE)
if(length(Args) != 6) {
  message("CABLE_plots.R requires the following command line arguments: 1) site, 2) startyear, 3) endyear, 4) outfile, 5) obs_dir, 6) plot_dir");quit()
} 

site      <- as.character(Args[1])
startyear <- as.numeric(Args[2])
endyear   <- as.numeric(Args[3])
outfile   <- as.character(Args[4])
obs_dir   <- as.character(Args[5])
plot_dir  <- as.character(Args[6])

source(paste0(plot_dir,"/CABLE_plots_functions.R"))


#### script assumes that observations are full years!
## Settings
model    <- 'CABLE'  # JSBACH or CABLE
obs_type <- unlist(strsplit(obs_dir,"/"))[length(unlist(strsplit(obs_dir,"/")))]
out_dir  <- dirname(outfile)
#seasonal <- TRUE     # plot mean diurnal cycle per season (TRUE) or for all months separately?

## Variables to plot
names <- read.csv(file=paste0(plot_dir,"/varnames_CABLE.csv"),header=TRUE,stringsAsFactors=F,na.strings="")
attach(names)



## some checks regarding variable lengths etc. 
if (length(varnames_sim) != length(varnames_obs)){
  stop("different number of variables in the simulation and observation lists!")
}
if (length(units) != length(varnames_sim)){
  stop("different length of units and variables!")
}
if (length(vartitles) != length(varnames_sim)){
  stop("different length of varnames and varnames_sim")
}


## open files
if (obs_type %in% c("OzFlux","OzFLUX","OZFLUX")){
  obs <- nc_open(paste0(obs_dir,"/",site,"_L6.nc"))
} else {
  stop("not yet implemented.")
}
sim <- nc_open(outfile)


time_sim <- ncvar_get(sim,varid="time")
if (model == "JSBACH"){
  time_sim <- time_sim * secperday
}
time_sim <- time_sim - (time_sim[2] - time_sim[1])
time_obs <- obs$dim$time$vals
fracs    <- ncvar_get(sim,varid="patchfrac")
ntiles   <- length(fracs)

## get time information and variables
dmy_sim  <- sec.to.dmy(time_sim,startyear,endyear)
vars_sim <- extract.vars(ncfile=sim,varnames=varnames_sim,fracs=fracs)
vars_sim <- cbind(vars_sim,dmy_sim)

## cut obs to simulations and simulations to obs!! (smallest common subset)
dmy_obs  <- dmy.obs(time_obs,origin='1800-01-01')
vars_obs <- extract.vars(ncfile=obs,varnames=varnames_obs,fracs=1)
vars_obs <- cbind(vars_obs,dmy_obs)

if ((length(unique(dmy_sim[,'hod'])) - length(unique(dmy_obs[,'hod']))) > 1.5){
  stop("observations and simulations must have the same frequency!")
}

vars_sim[vars_sim[,'time'] < min(dmy_obs[,'time']) | is.na(vars_sim[,'time']),] <- NA
vars_sim[vars_sim[,'time'] > max(dmy_obs[,'time']) | is.na(vars_sim[,'time']),] <- NA
vars_obs[vars_obs[,'time'] < min(dmy_sim[,'time']) | is.na(vars_obs[,'time']),] <- NA
vars_obs[vars_obs[,'time'] > max(dmy_sim[,'time']) | is.na(vars_obs[,'time']),] <- NA

## correct units
if (model=="JSBACH"){
  min1 <- c("latent_heat_flx","sensible_heat_flux")
  min2 <- c("CO2_flux_net","gross_assimilation_acc","carbox_rate_limited","e_transport_rate_limited","reco","ra")
  if (any(colnames(vars_sim) %in% min1)){
    vars_sim[,which(colnames(vars_sim) %in% min1)] <- vars_sim[,which(colnames(vars_sim) %in% min1)] * -1
  }
  if (any(colnames(vars_sim) %in% min2)){
    vars_sim[,which(colnames(vars_sim) %in% min2)] <- vars_sim[,which(colnames(vars_sim) %in% min2)] * 1e06
  }
}


## calculate mean diurnal courses by months/seasons
dmean_sim_month <- aggregate(vars_sim,by=list(vars_sim[,"hod"],vars_sim[,"month"]),mean)[,-c(1,2)]
dmean_obs_month <- aggregate(vars_obs,by=list(vars_obs[,"hod"],vars_obs[,"month"]),mean)[,-c(1,2)]

dmean_sim_season <- aggregate(vars_sim,by=list(vars_sim[,"hod"],vars_sim[,"season"]),mean)[,-c(1,2)]
dmean_obs_season <- aggregate(vars_obs,by=list(vars_obs[,"hod"],vars_obs[,"season"]),mean)[,-c(1,2)]


plot.model.obs(dmean_sim_season,dmean_obs_season,filename=paste0(out_dir,"/",site,"_plots_season.pdf"),agg="season")
plot.model.obs(dmean_sim_month,dmean_obs_month,filename=paste0(out_dir,"/",site,"_plots_month.pdf"),agg="month")


cat("plotting successfully finished!")