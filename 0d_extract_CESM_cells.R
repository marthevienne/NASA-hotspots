## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-21
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
library(raster)
library(ncdf4)
library(terra)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/functions_raster/netcdf_to_raster_depth.R")
source("~/Desktop/NASA-hotspots/functions_raster/raster_to_df.R")
source("~/Desktop/NASA-hotspots/useful_functions/extract_cells_netcdf.R")
## ---------------------------

#==================================================================
# 1) Zooplankton biomass
#==================================================================

file_nc <- "~/Dropbox/data/CESM_outputs/JRA 4p2z run/CESM-JRA-002branch-2004-2021.monthly.mesozooC.nc"
file_clip <- "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2021.tiff"
lon_name = "TLONG"
lat_name = "TLAT"
var_name = "mesozooC"
years <- seq(2004, 2021)
depths <- seq(1, 15)
months = seq(1, 12)
tolerance = 5.36362e-05

zoo_df <- extract_cells_netcdf(
  file_nc = file_nc, 
  file_clip = file_clip, 
  lon_name = lon_name,
  lat_name = lat_name,
  var_name = var_name,
  years = years, 
  depths = depths, 
  months = months,
  tolerance = tolerance
)

saveRDS(zoo_df, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mezozooC_polynya")

#==================================================================
# 2) NPP
#==================================================================

file_nc <- "~/Dropbox/data/CESM_outputs/JRA 4p2z run/CESM-JRA-002branch-2004-2021.monthly.photoC_TOT_zint.nc"
file_clip <- "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2021.tiff"
lon_name = "TLONG"
lat_name = "TLAT"
var_name = "" # ??????????
years <- seq(2004, 2021)
depths <- 1
months = seq(1, 12)
tolerance = 5.36362e-05

NPP_df <- extract_cells_netcdf(
  file_nc = file_nc, 
  file_clip = file_clip, 
  lon_name = lon_name,
  lat_name = lat_name,
  var_name = var_name,
  years = years, 
  depths = depths, 
  months = months,
  tolerance = tolerance
)

saveRDS(NPP_df, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.photoC_TOT_zint_polynya")

#==================================================================
# 3) MLD
#==================================================================

file_nc <- "~/Dropbox/data/CESM_outputs/JRA 4p2z run/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR.nc"
file_clip <- "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2021.tiff"
lon_name = "TLONG"
lat_name = "TLAT"
var_name = "HMXL_DR"
years <- seq(2004, 2021)
depths <- 1
months = seq(1, 12)
tolerance = 5.36362e-05

nc_open(file_nc)

MLD <- extract_cells_netcdf(
  file_nc = file_nc, 
  file_clip = file_clip, 
  lon_name = lon_name,
  lat_name = lat_name,
  var_name = var_name,
  years = years, 
  depths = depths, 
  months = months,
  tolerance = tolerance
)

MLD_df <- MLD[[2]]
MLD_r <- MLD[[1]]

saveRDS(MLD_df, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR_polynya")
terra::writeRaster(MLD_r, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR_polynya.tiff", overwrite = T)

## End script
rm(list=ls())















nc_open(file_name)

zoo_r <- netcdf_to_raster(
  file_name,
  lon_name,
  lat_name,
  depth_name,
  var_name,
  n_years,
  n_depths,
  n_months,
  tolerance
)

lyr_names <- paste0(rep(rep(month.name, each = length(depth)), length(years)), ".", 
                    rep(years, each = 12 * length(depth)), ".", 
                    rep(depth, (length(years) * 12)))
length(lyr_names)
names(zoo_r) <- lyr_names

zoo_new <- rast(zoo_r)
names(zoo_new) <- lyr_names
terra::writeRaster(zoo_new, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mesozooC.tiff", overwrite = T)

#------------------------------------------------------------------
# Extract cells that fall into polynyas cells
#------------------------------------------------------------------
rm(list=ls())

pol_r <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2021.tiff")
plot(pol_r$January.2021)

zoo_r <- brick("~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mesozooC.tiff")
names(zoo_r)

i = 1
r_new <- NULL

for (lyr in names(zoo_r)) {
  print(lyr)
  
  lyr_zoo <- which(names(zoo_r) == lyr)
  z <- zoo_r[[lyr_zoo]] #___zooplankton layer
  
  lyr_month <- str_extract(lyr, "[A-z]*.[0-9]*")
  lyr_pol <- which(names(pol_r) == lyr_month)
  p <- pol_r[[lyr_pol]] #___polynya layer
  p[p == 2] = 1
  
  z <- crop(z, p)
  
  r_pol_z <- z * p
  
  r_new[[i]] <- r_pol_z
  
  i = i + 1
}

r_new <- do.call(brick, r_new)
r_new <- rast(r_new)
names(r_new) <- lyr_names
terra::writeRaster(r_new, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mesozooC_polynya.tiff", overwrite = T)

#==================================================================
# 2) MLD (monthly) 
#==================================================================

years <- seq(2004, 2021)
depth <- seq(1, 15)

file_name <- "~/Dropbox/data/CESM_outputs/JRA 4p2z run/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR.nc"
nc_open(file_name)

lon_name = "TLONG"
lat_name = "TLAT"
var_name = ""
n_depths = 1
tolerance = 5.36362e-05
n_years = length(years)
n_months = 12

MLD_r <- netcdf_to_raster(
  file_name,
  lon_name,
  lat_name,
  depth_name,
  var_name,
  n_years,
  n_depths,
  n_months,
  tolerance
)

lyr_names <- paste0(rep(rep(month.name, each = length(depth)), length(years)), ".", 
                    rep(years, each = 12 * length(depth)), ".", 
                    rep(depth, (length(years) * 12)))
length(lyr_names)
names(MLD_r) <- lyr_names

MLD_new <- rast(MLD_r)
names(MLD_new) <- lyr_names
terra::writeRaster(MLD_new, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR.tiff", overwrite = T)

#------------------------------------------------------------------
# Extract cells that fall into polynyas cells
#------------------------------------------------------------------
rm(list=ls())

pol_r <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2021.tiff")
plot(pol_r$January.2021)

r <- brick("~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR.tiff")
names(r)

i = 1
r_new <- NULL

for (lyr in names(r)) {
  print(lyr)
  
  lyr_r <- which(names(r) == lyr)
  z <- r[[lyr_r]] #___zooplankton layer
  
  lyr_month <- str_extract(lyr, "[A-z]*.[0-9]*")
  lyr_pol <- which(names(pol_r) == lyr_month)
  p <- pol_r[[lyr_pol]] #___polynya layer
  p[p == 2] = 1
  
  z <- crop(z, p)
  
  r_pol_z <- z * p
  
  r_new[[i]] <- r_pol_z
  
  i = i + 1
}

r_new <- do.call(brick, r_new)
r_new <- rast(r_new)
names(r_new) <- lyr_names
terra::writeRaster(r_new, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR_polynya.tiff", overwrite = T)

#==================================================================
# 3) Transform to df 
#==================================================================

zoo_r_clip <- brick("~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mesozooC_polynya.tiff")
df_zoo <- raster_to_df(zoo_r_clip)

saveRDS(df_zoo, "~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.mezozooC_polynya")

mld_r_clip <- brick("~/Desktop/WHOI/Data/CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR_polynya.tiff")
df_mld <- raster_to_df(mld_r_clip)

## End script
rm(list=ls())
