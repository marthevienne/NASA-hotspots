##
## Script name: bathymetry_preprocessing.R
##
## Purpose of script: Transform GEBCO topography raster to bathymetry raster
##                    1) Get bathymetry from GEBCO topography
##                    2) Example: project bathymetry and continent in more suited CRS (Coordinate Reference System)
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-01
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
setwd("~/Desktop/WHOI/Data/bathy_data/")

## Library
library(raster)
library(marmap)
library(ggfortify)
library(terra)
library(tidyterra)

#==================================================================
# 1) GET BATHYMETRY FROM GEBCO TOPOGRAPHY
#==================================================================

## Topographic data: GEBCO 2023 East Antarctica
r <- rast("GEBCO_09_Jun_2023_c176bcb0db4a/gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.tif")
r[r > 0] <- NA
r #___check extent and resolution
writeRaster(r, "RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd", overwrite = T)

## Antarctic continent
bbox <- ext(-5, 170, -80, -58.5)
antarctica <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)

## Map bathymetry East Antarctica
ggplot() +
  geom_spatraster(data = r) + #___bathymetry
  geom_spatvector(data = antarctica) + #___continents
  scale_fill_hypso_c(na.value = NA, palette = "arctic_bathy") + #___color palette
  theme_minimal()

ggsave("GEBCO_09_Jun_2023_c176bcb0db4a/bathy_EA.png", height = 42, width = 40, units = c("cm"), dpi = 600)

#==================================================================
# 2) REPROJECT BATHYMETRY AND CONTINENTS
#==================================================================

r <- rast("RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd")

proj <- crsuggest::suggest_crs(r) #___suggest Coordinate Reference System
dest_proj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +x_0=6000000 +y_0=6000000 +datum=WGS84 +units=m +no_defs"
r_proj <- project(r, dest_proj) #___project bathymetry in destination CRS

writeRaster(r_proj, "EPSG3032_RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd", overwrite = T)
# saveRDS(r_proj, "~/Desktop/WHOI/Data/bathy_data/EPSG3032_RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0")

## Antarctic continent
bbox <- ext(-180, 180, -90, -58.5)
antarctica <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox) |>
  project(dest_proj)

## Map bathymetry East Antarctica
ggplot() +
  geom_spatraster(data = r_proj) + #___bathymetry
  geom_spatvector(data = antarctica) + #___continents
  scale_fill_hypso_c(na.value = NA, palette = "arctic_bathy") + #___color palette
  theme_minimal()

ggsave("GEBCO_09_Jun_2023_c176bcb0db4a/proj_bathy_EA.png", height = 42, width = 40, units = c("cm"), dpi = 600)

## End script
rm(list=ls())
