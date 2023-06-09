# ------------------------------------------------------------------------------
# ------------------------ Complete shelf/slope table --------------------------

rm(list=ls())

setwd("C:/Users/lucie/Dropbox/codes/scripts_stage_Lucie/R")
table=read.table("~/Desktop/WHOI/Data/bathy_data/shelf_slope.txt",sep="\t",head=T) 
table=table[-nrow(table),]

# Create longitude vector of good size
lon=seq(-1,158,0.5)

# Load the bathy
library(raster)

bathy <- raster("~/Dropbox/data/bathymetry/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")
show(bathy)

plot(bathy)

# Extract depth for a given longitude
ext <- extent(bathy) # Dimensions of the raster

lon = 158

points_lon <- rep(lon, 3840) #nrow in show(bathy)
by = (ext@ymax-ext@ymin)/3839
points_lat <- seq(ext@ymin,ext@ymax,by=abs(by))

points <- cbind(points_lon,points_lat)

depth <- extract(bathy,points)

# plot latitude/depth and determine the end of the shelf
plot(points_lat,depth, main = paste("Longitude = ", lon), type = "l", xlim = c(-69,-66))

abline(v=points_lat[1380], col="red")
abline(v=points_lat[1373], col="blue")
abline(v=points_lat[1360], col="green")

lat1 <- points_lat[1373] # depends on the considered longitude
value1 <- depth[1373] # depends on the considered longitude

# Determine the end of the slope (first lat below 3500 meters depth)
depth_lim <- 3500
abline(h = depth_lim, col="orange")

a <- which(depth < (- depth_lim))
lat2 <- points_lat[a[1]]
value2 <- depth[a[1]]
  
# Add values to the original table
data <- data.frame("lat1" = 0, "value1" = 0, "lat2" = 0, "value2" = 0)
data$lat1 <- lat1
data$value1 <- value1
data$lat2 <- lat2
data$value2 <- value2

table <- rbind(table, data)

write.table(table, file = "C:/Users/lucie/Dropbox/codes/scripts_stage_Lucie/R/shelf_slope_158.txt" ,sep=",",row.names=F)
  
  
  
  
  