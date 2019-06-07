###################
#### Functions ####
###################

## Parameters
library(lubridate)
library(ncdf4)
secperday <- 86400

plot.model.obs <- function(dmean_sim,dmean_obs,filename,agg){
  
  col.obs  <- "black"
  col.sim  <- "red2"
  cex.pt   <- 0.8
  lwd.pt   <- 1
  tcl      <- -0.2
  
  if (agg == "month"){
    pdf(file=filename,onefile=TRUE,width=8,height=8)
  } else {
    pdf(file=filename,onefile=TRUE,width=8,height=6)
  }
  for (i in seq_along(varnames_sim)){
    
    ylim <- c(min(dmean_obs[,i],dmean_sim[,i],na.rm=T),max(dmean_obs[,i],dmean_sim[,i],na.rm=T))
    
    if (agg == "month"){
      
      sim  <- dmean_sim[,i]
      obs  <- dmean_obs[,i]
      
      lin  <- pretty(c(sim,obs))
      lg   <- length(sim)
      xlim <- c(0.02*lg,lg-0.02*lg)
      
      split.screen(c(2,1))
      split.screen(c(1,2),screen=2)
      screen(1)
      par(mar=c(2,5,2.5,0.5))
      
      plot(sim,col=col.sim,type='l',axes=FALSE,ylim=ylim,ylab="",xlab="",xlim=xlim)
      abline(h=pretty(c(sim,obs)),lty=2,col='grey80')
      points(obs,col=col.obs,type='l',cex=cex.pt)
      points(obs,col=col.obs,pch=1,lwd=lwd.pt,cex=cex.pt)
      points(sim,col=col.sim,type='p',pch=16,cex=cex.pt)
      
      
      axis(1,at=seq(lg/12,lg,lg/12),labels=F)
      mtext(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),at=seq(lg/12,lg,lg/12) - 0.5*lg/12,line=0.7,side=1)
      axis(2,at=NULL,labels=TRUE,las=2)
      box()
      legend(x=-2,y=1.05*ylim[2],legend=c(model,obs_type),col=c(col.sim,col.obs),pch=c(16,1),bty="n",ncol=2,x.intersp=0.7,cex=1.2)
      
      mtext(paste0("Per Hour Monthly ",vartitles[i], " (",startyear,"-",endyear,")"),
            side=3,line=0.8,cex=1.5)
      #mtext("Month",side=1,line=2.2,cex=1.5)
      mtext(paste0(vartitles[i]," (",units[i], ")"),side=2,line=3,cex=1.4)
      
      
      screen(3)
      par(mar=c(4,4,3,0.5))
      plot(sim ~ obs,ylim=ylim,xlim=ylim,ylab=model,xlab=obs_type,cex.lab=1.3,mgp=c(2.5,0.6,0),
           las=1,cex=cex.pt)
      mtext(paste0(vartitles[i]," (",units[i], ")"),side=3,line=1,cex=1.5)
      abline(0,1)
      
      screen(4)
      print('not yet implemented. Open for suggestions...')
      
      close.screen(all.screens=TRUE)
      
      
      
      
      
      
    } else if (agg == "season"){
      
      lin  <- pretty(c(dmean_sim[,i],dmean_obs[,i]))
      lg   <- length(dmean_sim[,i])/4
      xlim <- c(0.02*lg,lg-0.01*lg)
      
      split.screen(c(2,2))
      
      
      for (j in 1:4){
        screen(j)
        
        par(mar=c(3,2.5,0.5,0.5),oma=c(0,0,2,0))
        
        sim <- dmean_sim[dmean_sim[,"season"] == j,i]
        obs <- dmean_obs[dmean_obs[,"season"] == j,i]
        
        if (length(sim) > 24.1){
          hour <- seq(0.5,24,0.5)
        } else {
          hour <- seq(1,24,1)
        }
        
        plot(sim ~ hour,col=col.sim,type='l',axes=FALSE,ylim=ylim,ylab="",xlab="Hour",xlim=xlim,mgp=c(1.6,0.35,0))
        abline(h=lin,lty=2,col='grey80')
        points(obs ~ hour,col=col.obs,type='l',cex=cex.pt)
        points(obs ~ hour,col=col.obs,pch=1,lwd=lwd.pt,cex=cex.pt)
        points(sim ~ hour,col=col.sim,type='p',pch=16,cex=cex.pt)
        
        axis(1,at=1:length(sim),labels=T,tcl=tcl,mgp=c(2.5,0.5,0))
        axis(2,at=NULL,labels=TRUE,las=2,tcl=tcl,mgp=c(2.5,0.5,0))
        
        
        if (j == 1){
          leg.text <- "DJF"
          mtext(paste0(vartitles[i]," (",units[i], ")"),side=3,line=1,cex=1.3,at=25)
        } else if (j == 2){
          leg.text <- "MAM"
        } else if (j == 3){
          leg.text <- "JJA"
        } else if (j == 4){
          leg.text <- "SON"
        }
        
        legend(x=-2,y=1.05*ylim[2],legend=leg.text,bty="n",cex=1.4)
        box()
        
      }
      
      close.screen(all.screens=TRUE)
      
    }
    
    
  }
  dev.off()
  
}







# time in seconds since beginning of simulation period

sec.to.dmy <- function(time,startyear,endyear){
  tstep    <- time[2] - time[1]
  day_frac <- time / secperday
  day_tot  <- ceiling(day_frac)
  nr_years <- round(day_tot[length(day_tot)]/365.25)
  years    <- c(startyear:(startyear + nr_years - 1))
  if (years[length(years)] != endyear){
    warning("check calculation of years! Something went wrong...")
  }
  doy = day = month = year <- integer()
  for (yr in years){
    
    if (leap_year(yr)){
      dpm <- c(31,29,31,30,31,30,31,31,30,31,30,31)
      
      year  <- c(year,rep(yr,366*secperday/tstep))
      doy   <- c(doy,sort(rep(1:366,secperday/tstep)))
      day   <- c(day,unlist(sapply(1:12,function(x) sort(rep(1:dpm[x],secperday/tstep)))))
      month <- c(month,unlist(sapply(1:12,function(x) rep(x,dpm[x]*secperday/tstep))))
      
      
    } else {
      dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      
      year  <- c(year,rep(yr,365*secperday/tstep))
      doy   <- c(doy,sort(rep(1:365,secperday/tstep)))
      day   <- c(day,unlist(sapply(1:12,function(x) sort(rep(1:dpm[x],secperday/tstep)))))
      month <- c(month,unlist(sapply(1:12,function(x) rep(x,dpm[x]*secperday/tstep))))
    }
    
  }
  
  season <- rep(NA_integer_,length(month))
  season[month %in% c(12,1,2)] <- 1
  season[month %in% c(3,4,5)] <- 2
  season[month %in% c(6,7,8)] <- 3
  season[month %in% c(9,10,11)] <- 4
  
  if (tstep > 1801){
    hod <- rep(c(seq(0,(23),1)),day_tot[length(day_tot)])
  } else {
    hod <- rep(c(seq(0,(23.5),0.5)),day_tot[length(day_tot)])
  }
  
  
  hour <- floor(hod)
  minute <- ifelse(hod %% 1 == 0,0,30)
  
  time <- ymd_hms(paste(as.character(year),as.character(month),as.character(day),
                        as.character(hour),as.character(minute),"0",sep="-")) 
  
  return(data.frame(year,season,month,doy,hod,time))
}




dmy.obs <- function(time_obs,origin='1800-01-01'){
  
  ## The timesteps are shifted one forward to match the simulated timesteps!!
  # this is done in the calculation of the date now!
  # The Ozflux convention is that the time step denotes the end of the time period
  # time_obs <- time_obs - (time_obs[2] - time_obs[1])
  
  tstep <- round((time_obs[2] - time_obs[1])*secperday)
  date  <- as.Date(time_obs - tstep/secperday, origin=origin, format="%Y-%m-%d")  # subtract time step here to avoid that 0 hour is the previous day!!
  
  year  <- as.numeric(substr(date,1,4))
  month <- as.numeric(substr(date,6,7))
  day   <- as.numeric(substr(date,9,10))
  doy   <- as.numeric(format(date, "%j"))
  
  season <- rep(NA_integer_,length(month))
  season[month %in% c(12,1,2)]  <- 1
  season[month %in% c(3,4,5)]   <- 2
  season[month %in% c(6,7,8)]   <- 3
  season[month %in% c(9,10,11)] <- 4
  
  
  ## TODO: simplify the following lines!!
  nr_days <- length(unique(as.character(date)))
  if (tstep > 1801){
    hod <- rep(c(seq(0,(23),1)),nr_days)
  } else {
    hod <- rep(c(seq(0,(23.5),0.5)),nr_days)
  }

  
  del <- integer()
  
  start_missing <- round((signif((time_obs[1]- tstep/secperday),nchar(as.character(time_obs[1]))+3) %% 1) * (secperday/tstep)) 
cat("start missing:",start_missing,fill=TRUE)
  if (start_missing > 0){
    del <- c(1:start_missing)
  }
  
  end_missing   <- 0
  if (time_obs[length(time_obs)] %% 1 != 0){
    end_missing   <- round((1 - (time_obs[length(time_obs)] %% 1)) * (secperday/tstep))
  } 
cat("end missing:",end_missing,fill=TRUE)
  if (end_missing > 0){
    del <- c(del,c((length(hod)-end_missing+1) : length(hod)))
  }
  
  if (length(del) > 0){
    hod <- hod[-c(del)]
  }


  hour   <- floor(hod)
  minute <- ifelse(hod %% 1 == 0,0,30)
  
  
  time <- ymd_hms(paste(as.character(year),as.character(month),as.character(day),
                        as.character(floor(hour)),as.character(minute),"0",sep="-")) 
  
  return(data.frame(year,season,month,doy,day,hod,minute,time))
  
}




extract.vars <- function(ncfile,varnames,fracs){
  
  length <- ncfile$dim$time$len
  hourly <- data.frame(matrix(NA_real_,nrow=length,ncol=length(varnames)))
  colnames(hourly) <- varnames
  
  for (var in varnames){
    if (is.na(var)) next
    hourly_tiles <- ncvar_get(ncfile,var)
    if (length(dim(hourly_tiles)) < 2){   # one tile only
      hourly[,var] <- hourly_tiles
    } else {
      hourly[,var] <- apply(hourly_tiles,2,function(x) sum(x*fracs))
    }
  }
  
  return(hourly)
}
