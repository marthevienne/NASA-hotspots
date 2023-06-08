##############################################################################
#Calcul de la bathy, de la pente et extraction des datas sous les echantillons

rm(list=ls())
# install.packages("sp")
# install.packages("rgdal")
# install.packages("ncdf")
# install.packages("raster")

library(raster)
library(sp)
library(rgdal)
library(marmap)

#1) Bathy et pente
r2<- raster("~/Dropbox/data/bathymetry/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")
#r2<- raster("/Users/saralabrousse/Dropbox/CNRS/NASA_polynya/data/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")

#str(r2) pour voir sa structure
plot(r2)  # NB la fonction plot ne marche pas pour des fichiers plus gros

#creation d'une fonction pour ne prendre que la bathy (et pas les élévations)
fun <- function(x) { x[x > 0] <- NA; return(x) }
bathy <- calc(r2, fun)

plot(bathy)


#########################################

#2) Extraction des datas for dive-----------------------------------------------
#extraction de données à partir de positions
dv=read.table("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters.txt",sep=",",head=T)
dv=read.table("/Users/saralabrousse/Dropbox/CNRS/NASA_polynya/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters.txt",sep=",",head=T)
dv$date_fin=as.POSIXct(dv$date_fin,origin="1970-01-01",tz="GMT")

dv$bathy=NA

for (i in 1:nrow(dv)){
  #print(i)
  dt=dv[i,]
  lonlat=cbind(dt$lon,dt$lat)  #à remplacer par ton vecteur de lon et lat  (les 200)
  dv$bathy[i] <- extract(r2, lonlat)
}

#write.table(dv,"C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/filtered_dive_ses_polynya_5meters_bathy.txt",sep=",",row.names=F)
write.table(dv,"/Users/saralabrousse/Dropbox/CNRS/NASA_polynya/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy.txt",sep=",",row.names=F)

# #2) Extraction des datas for CTD------------------------------------------------
# #extraction de données à partir de positions
# seal <-read.table("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/1_behavior_data/10_balise_reinterpol_SSM_modif14414.txt",sep=",",head=T)
# seal$date<-as.POSIXct(seal$date,origin="1970-01-01",tz="GMT")
# 
# seal$bathy=NA
# data <- data.frame()
# unik=unique(seal$seal)
# 
# for (p in 1:length(unik)){
#   bal <- seal[seal$seal==unik[p],]
# 
#   ctd <-unique(bal$nump)
# 
#   for (i in 1:length(ctd)){
#     bal2 = bal[bal$nump==ctd[i],]
#     lonlat=cbind(bal2$lon[1],bal2$lat[1])  #à remplacer par ton vecteur de lon et lat  (les 200)
#     bal$bathy[bal$nump==ctd[i]] <- as.numeric(extract(r2, lonlat))
#   }
#   data<-rbind(data,bal)
# }
# seal=data
# write.table(seal,"/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/1_behavior_data/10_balise_reinterpol_SSM_bathy_modif14414.txt",sep=",",row.names=F)
