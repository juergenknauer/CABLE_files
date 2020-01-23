### create site list for PEST runs

setwd("C:/Users/kna016/Documents/CABLE/")

## load existing site list

sitelist <- read.table("OzFLUX_sitelist_v5.txt",header=TRUE,sep="\t")

sitelist[,"startmm"]  <- rep(01,nrow(sitelist))
sitelist[,"endmm"]    <- rep(12,nrow(sitelist)) 
sitelist[,"patch"]    <- rep('2',nrow(sitelist))
sitelist[,'sitecode'] <- c("Ade","Ali","Cpr","Cpt","Cow","Cum","Dap","Dau","Dry","Eme","Fog","Gin",
                           "Gww","How","Lit","Otw","Rdm","Rid","Rig","Rob","Sam","Stu","Tit","Tum",
                           "War","Whr","Wsf","Yan")

sitelist_PEST <- sitelist[,c('sitecode','X','startmm','startyear','endmm','endyear','latitude','longitude','patch')]
colnames(sitelist_PEST) <- c('sitecode','sitename','startmm','startyy','endmm','endyy','lat','lon','patch')


write.table(sitelist_PEST,file="./PEST/OzFluxSites.txt",sep=',',quote=TRUE,row.names=FALSE)
