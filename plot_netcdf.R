## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-30
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
library(rasterVis)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/functions_raster/netcdf_to_raster.R")
## ---------------------------
filename <- "~/Desktop/WHOI/Data/output_data/residence_time/RES_CESM_LR_time_spent.nc"

# Transform netcdf to raster
r <- netcdf_to_raster(
  file_name = filename,
  lon_name = "lon",
  lat_name = "lat",
  var_name = "tot_time_spent",
  n_years = 1,
  n_months = 4, tolerance = 0.0001) 

gplot(r) + 
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225)), na.value = NA) +
  coord_equal() +
  theme_map()

## End script
rm(list=ls())
