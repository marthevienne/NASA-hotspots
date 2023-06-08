rm(list =ls())

library(ggplot2)
library(rgdal)
library(marmap)
#library(viridis)
#library(paletteer)
library(scales)
library(ggplot2)
library(sp)
library(raster)
library(paletteer)
library(graticule)

#topo2 <- raster("/Users/saralabrousse/Dropbox/CNRS/DDU_2023/1_plot_DDU_R/gvdem_11May11.tif") 
topo2 <- raster("/Users/saralabrousse/Dropbox/CNRS/DDU_2023/1_plot_DDU_R/gvdem_arc/gvdem100v3/w001001.adf") 

#===============================================================================
#===================LARGE MAP OF THE AREA BATHY=================================

largetopo <-crop(topo2, extent(138, 142, -66.8, -65))
largetopo2 = as.bathy(largetopo)
largetopo2[largetopo2 >=0] <-NA
#writeRaster(largetopo, "/Volumes/Weddell1182/manip/map_DDU/largetopo.grd")

dv1=read.csv("/Users/saralabrousse/Dropbox/CNRS/DDU_2023/1_plot_DDU_R/rapport_mission_crabiers/194898-Locations.csv",sep=";")
dv1$lat <- as.numeric(dv1$Latitude)
dv1$lon <- as.numeric(dv1$Longitude)
a=which(dv1$Quality=="B")
dv1=dv1[-a,]
dv1$date = as.Date(dv1$Date, format = "%d/%m/%Y %H:%M:%S")
dv1$date <- as.POSIXct(dv1$date,origin="1970-01-01",tz="GMT")

# pt1<-dv1[,c(19,20)]
# pt1=na.omit(pt1)
# coordinates(pt1) <- ~lon+lat
# projection(pt1) <- projection(largetopo)

#DDU location
DDU_loc=NULL
DDU_loc$lat[1]=-66.66
DDU_loc$lon[1]=140.01
DDU_loc=as.data.frame(DDU_loc)

library(base)
labels=pretty(dv1$date,6)
autoplot(largetopo2, geom=c("raster","contour"),coast=TRUE, 
         mapping = aes(levels(bins=30)))+
  geom_point(data = as.data.frame(DDU_loc), aes(x=lon,y=lat,col = DDU_loc), #__________ DDU location
             shape = 23, colour = "blue",fill="darkred", size = 4,
             show.legend = TRUE)+
  # geom_point(data = as.data.frame(dv1), aes(x=lon,y=lat), #__________ deployment location
  #            shape = 20, colour = "yellow", size = 2,
  #            show.legend=TRUE) +
  geom_point(data = as.data.frame(dv1), aes(lon, lat,  colour = date,  fill = NULL),size=3,show.legend = TRUE) +
  scale_colour_gradient(low="green",high="red",breaks=(labels),labels=format(labels,"%m/%d"))+
  
  #geom_point(data = as.data.frame(coast), aes(x=lon,y=lat),size=0.01)+
  #geom_text(data = as.data.frame(deploy_loc), aes(x = (lon+0.5), y = (lat), label = "Deployment site"), size = 5.5) +
  geom_text(data = as.data.frame(DDU_loc), aes(x = (lon-0.5), (y = lat), label = "Station location"), size = 5.5) +
  labs(x="Longitude", y="Latitude",colour="Date",fill='Bathymetry (m)')+
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=17,face="bold"), #__________ legend texts
        legend.title=element_text(size=17,face="bold"))+ #__________ legend titles
  geom_rect(xmin = 139.9, xmax = 140.21, ymin = -66.69, ymax = -66.585, 
            fill = NA, colour = "black", size = 0.5) 

ggsave("/Users/saralabrousse/Desktop/Figure_bathy_crabiers_2022.png",height=42,width=40,units=c("cm"),dpi=300)

