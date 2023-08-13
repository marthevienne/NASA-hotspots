## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-26
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
## ---------------------------
## Library
library(ncdf4)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/functions_raster/netcdf_to_raster.R")
## ---------------------------

## CESM data
zoo <- "~/Dropbox/data/CESM_outputs/JRA 4p2z run/CESM-JRA-002branch-2004-2021.monthly.mesozooC.nc"
nc_open(zoo)

## Polynya cells
crop <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2019.tiff")


## End script
rm(list=ls())
