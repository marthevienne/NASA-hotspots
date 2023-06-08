##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-08
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes: À FINIR 
##   
##
## ---------------------------
rm(list=ls())

## Working directory
setwd("~/Dropbox/data/polynya_contours_NCAR_2023/")

## Set output directory
path_output = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")

## Library
library(ncdf4)

## Extract polynya contours from raster to polygons

## Read netcdf data
file1 <- "ssmi_cdr_typical_polynya_mask_85%thresh"
nc_typical_polynyas <- nc_open(paste0(file1, ".nc"))

## Get variables for typical polynyas: lon, lat, time, 3D matrix of polynyas
lon <- ncvar_get(nc_typical_polynyas, varid = "TLONG_sub")[,1]
lat <- ncvar_get(nc_typical_polynyas, varid = "TLAT_sub")[1,]


matrix = readRDS("polynya_id_typical_all_map")
polynya_raster = raster(matrix$pol_id)

rotate1<-t(matrix$pol_id)
rotate2 <- rotate1[nrow(rotate1):1,]

lonCrop = lon[1:which(lon > 160)[which(diff(which(lon > 160)) != 1) + 1]]
lonCrop[which(lonCrop > 180)] = lonCrop[which(lonCrop > 180)] - 360
start = sort(which(lonCrop < -2), decreasing = T)[1] + 1
lonCrop = lonCrop[start:length(lonCrop)]

matrix_crop <- rotate2[, start:which(lon > 160)[which(diff(which(lon > 160)) != 1) + 1]]

polynya_raster <- raster(matrix_crop)
ext <- extent(min(lonCrop), max(lonCrop), min(matrix$lat), max(matrix$lat))
range(lonCrop)

extent(polynya_raster) <- ext
polynya_raster <- setExtent(polynya_raster, ext, keepres=TRUE)

polygons_pol = rasterToPolygons(polynya_raster, na.rm = T, n = 16, dissolve = T)

ggplot(polygons_pol, aes(x = long, y = lat, group = id)) + 
  geom_polygon(colour='black', fill ="NA")

saveRDS(polygons_pol, "polynyas_polygons")
