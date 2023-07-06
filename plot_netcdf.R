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
library(ggplot2)
library(ggthemes)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/functions_raster/netcdf_to_raster.R")
## ---------------------------
filename <- "~/Desktop/WHOI/Data/output_data/residence_time/RES_CESM_LR_nseals_sum_pixel_months.nc"
nc_open(filename)

# Transform netcdf to raster
r <- netcdf_to_raster(
  file_name = filename,
  lon_name = "lon",
  lat_name = "lat",
  # var_name = "tot_time_spent",
  var_name = "n_seals",
  n_years = 1,
  n_months = 12, tolerance = 0.0001) 

gplot(r) + 
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(terrain.colors(225)), na.value = NA) +
  coord_equal() +
  theme_map()

## End script
rm(list=ls())
