##
## Script name: extract_bathy_dives
##
## Purpose of script: 1) Extract bathymetry at each dive location
##                    2) Define shelf and slope regions
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-04
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
##
## Paths
path_input = "~/Desktop/WHOI/Data/"
path_output = "~/Desktop/WHOI/Data/output_data/"
##----------------------------

## Library
library(raster)

## Get script name (used for source run)
filename <- current_filename()
filename <- sub(".*/", "", filename)

## Import bathymetry data
bathy <- raster(paste0(path_input, "bathy_data/RES_0.0041_BATHY_sub_ice_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd"))
extent(bathy) #___check lon format (360 or 180)
# here, lon in -180->180. Lon have to be in the same referentiel as dive locations longitude

## Import dive data
dives <- readRDS(paste0(path_input, "output_data/interp_dive_metrics_V2"))
range(dives$START_LON)

n_dives = nrow(dives)
dives$bathy = NA
lonLat = cbind(dives$START_LON, dives$START_LAT)

## Extract bathymetry from raster layer at dives locations
dives$bathy = extract(bathy, lonLat)

## Save updated dive metrics table with bathymetry
saveRDS(dives, paste0(path_output, "interp_dive_metrics_V3"))
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "dive metrics updated with bathymetry saved: ", paste0(path_output, "interp_dive_metrics_V3")))
}

## End of script
rm(list=ls())
