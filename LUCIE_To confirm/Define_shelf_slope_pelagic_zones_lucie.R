##############################################################################
#3) definition zones dans le fichier dive
#----------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/lucie/Dropbox/codes/scripts_stage_Lucie/R")
table=read.table("shelf_slope_158.txt",sep=",",head=T) 
table$lon=seq(-0.5,158,0.5)
#table=table[-nrow(table),]


#----------------------------------------------------------------------------------------------------------

setwd("C:/Users/lucie/Dropbox/codes/data_SES_polynya")
dive <-read.table("filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_numpol21.txt",sep=",",head=TRUE)

# names(dive)<- c("Dist.SI_extent","Standard_deviation_SIC_50km","Average_SIC_50km","Dist.coast",
#                    "Trend_SIC_30days","Standard_deviation_SIC_30days",
#                    "SIC","SI_extent","Polynyas","Longitude","Latitude","Day_of_year","Hunting_time",
#                    "Year","Sex","Season","ID","Datenum","Maximal_dive_depth","Dive_duration","Bottom_time","flag",
#                    "solar_angle")
data=NULL

for (i in 1:length(dive$lon)){
  dv=dive[i,]
  #print(i)
  interp1 <- approx(table$lon,table$lat1,xout=dv$lon)
  interp2 <- approx(table$lon,table$lat2,xout=dv$lon)
  
  if(dv$lat <= interp1$y){ # Shelf
    dv$Zone <- 1
  }
  if (dv$lat > interp1$y & dv$lat <= interp2$y){ # Slope
    dv$Zone <- 2
  }
  if (dv$lat > interp2$y){ # Deep sea floor
    dv$Zone <- 3
  }
  
 data <-rbind(data,dv) 
}
data=as.data.frame(data)

write.table(data, file="filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_numpol21_shelf.txt",sep=",",row.names=F)

#data=read.table("ACP_tracksremoved_april15_inf50k_defshelf.txt",sep=",",head=T)


#plot de verif----------------------------------------------------------------------------------------------

## Ouverture fichier ? v?rifier
data <- read.table("/run/media/lbourreau/Elements/Stage_Lucie/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult_shelf.txt",sep=",",head=T)

## Ouverture bathy

library(ncdf4)
library(RNetCDF)
library(raster)

#Visualiser les variables, dÃÂÃÂ©finir la bathymÃÂÃÂ©trie

ddu <- nc_open("/run/media/lbourreau/Elements/Stage_Lucie/Bathy/gebco_08_-2_-74_150_-45.nc") 
ncvar_get(ddu,1)
var.get.nc(ddu,2)
var.get.nc(ddu,3)
var.get.nc(ddu,4)
bathy <- raster("/run/media/lbourreau/Elements/Stage_Lucie/Bathy/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")


# CrÃÂÃÂ©ation de tous les points de longitude et de latitude
longitude <- seq(-2,158,(1/60)) 
latitude <- seq(-45,-72,-(1/60)) 

### Plot

#Ranger dans l'ordre croissant les longitudes, et supprimer les valeurs identiques
long <- sort(unique(longitude))
lat <- sort(unique(latitude))
b <- bathy
dim(b) <- c(length(long), length(lat))
#derniÃÂÃÂ¨re colonne passe 1ere colonne
b <- b[,ncol(b):1]

#---------------------------------------
data$Lon=data$Longitude
data$Lat=data$Latitude
a=which(data$Zone==1)
data2=data[a,]

tiff(paste("C:\\Users\\Sarah\\Desktop\\verif_shelf.tiff"),res=700,height=14,width=24,units="cm",compression="lzw")
  par(mar=c(4.2,4.2,1.4,6))
  image(long, lat, b,xlab="Longitude (°E)",ylab="Latitude (°S)",col = grey(0:10/12),xlim=c(-2,150),zlim=c(-6600,0),ylim=c(-72,-45))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=4,drawlabels=T,col="grey", xlim=c(15,150) ,ylim=c(-72,-45),zlim=(c(-6600,0)))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=1,drawlabels=T,col="gray32", xlim=c(15,150),ylim=c(-72,-45),zlim=(c(-2000,-1000)))
  points(data2$Lon,data2$Lat, pch=20,cex=0.8,col="red")
   dev.off() 

a=which(data$Zone==2)
data2=data[a,]

tiff(paste("C:\\Users\\Sarah\\Desktop\\verif_slope.tiff"),res=700,height=14,width=24,units="cm",compression="lzw")
  par(mar=c(4.2,4.2,1.4,6))
  image(long, lat, b,xlab="Longitude (°E)",ylab="Latitude (°S)",col = grey(0:10/12),xlim=c(-2,150),zlim=c(-6600,0),ylim=c(-72,-45))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=4,drawlabels=T,col="grey", xlim=c(15,150) ,ylim=c(-72,-45),zlim=(c(-6600,0)))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=1,drawlabels=T,col="gray32", xlim=c(15,150),ylim=c(-72,-45),zlim=(c(-2000,-1000)))
  points(data2$Lon,data2$Lat, pch=20,cex=0.8,col="red")
   dev.off() 

a=which(data$Zone==3)
data2=data[a,]

tiff(paste("C:\\Users\\Sarah\\Desktop\\verif_pelagic.tiff"),res=700,height=14,width=24,units="cm",compression="lzw")
  par(mar=c(4.2,4.2,1.4,6))
  image(long, lat, b,xlab="Longitude (°E)",ylab="Latitude (°S)",col = grey(0:10/12),xlim=c(-2,150),zlim=c(-6600,0),ylim=c(-72,-45))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=4,drawlabels=T,col="grey", xlim=c(15,150) ,ylim=c(-72,-45),zlim=(c(-6600,0)))
  contour(as.numeric(long), as.numeric(lat),b, add=T, nlevels=1,drawlabels=T,col="gray32", xlim=c(15,150),ylim=c(-72,-45),zlim=(c(-2000,-1000)))
  points(data2$Lon,data2$Lat, pch=20,cex=0.8,col="red")
   dev.off() 

