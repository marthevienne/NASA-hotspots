##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-08
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
setwd("~/Desktop/WHOI/Data/output_data/")
## ---------------------------
## Library
library(raster)
## ---------------------------

## Import bathymetry data
bathy <- raster("~/Desktop/WHOI/Data/bathy_data/RES_0.0041_BATHY_sub_ice_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
bathy #___check extent

## Import dive data
dives <- readRDS("dive_metrics_V3")

dives$bathy = NA
latlon = cbind(dives$START_LON, dives$START_LAT)

dives$bathy = extract(bathy, latlon)

summary(dives$bathy) #___check number of NA
length(which(is.na(dives$bathy) | dives$bathy == 0))/nrow(dives)*100 #___fraction of dives on land

## Save dive metrics with bathymetry
saveRDS(dives, "dive_metrics_V4")

## End script
rm(list=ls())
