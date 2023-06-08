##
## Script name: extract_bathy_dives
##
## Purpose of script: 1) Extract bathy from GEBCO bathymetry at each dive location
##                    2) Define shelp and slope regions
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

## Working directory
setwd("~/Desktop/WHOI/Data/")

## Library
library(raster)

## Import elevation data
topo_bathy <- raster("~/Dropbox/data/bathymetry/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")

fun <- function(x) { x[x > 0] <- NA; return(x) }
bathy <- calc(topo_bathy, fun)

#plot(bathy)

# Check lon format (360 or 180)
extent(bathy)
#extent: -2, 160, -74, -58  (xmin, xmax, ymin, ymax) -> here, lon in -180->180. Lon have to be in the same referentiel as dive locations longitude

## Import dive data
dives <- readRDS("filtered_dives_north_boundary_5meters")
range(dives$START_LON)

n_dives = nrow(dives)
dives$bathy = NA
latlon = cbind(dives$START_LON, dives$START_LAT)

## Extract bathymetry from raster layer at dives locations
dives$bathy = extract(bathy, latlon)

## Save updated dive summary with bathymetry
saveRDS(dives, "filtered_dives_north_boundary_5meters_bathy")