##
## Script name: polynya_contours
##
## Purpose of script: 1) Open and rearrange polynya contours data from NCAR (netcdf) for SSMI and CESM data
##                        => create list object with lon, lat, z (NA or 1)
##                    2) Plot polynyas map
##                    3) Identify polynya
##
## Author: Marthe Vienne
##
## Date Created: 2023-04-25
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
rm(list = ls())

## Library
library(ncdf4) # for netcdf manipulation
library(lubridate)
library(raster)
library(dplyr)

## Functions
file.sources = list.files(path = "~/Desktop/WHOI/Codes/functions_polynyas/", pattern="*.R", full.names = T)
sapply(file.sources, source, .GlobalEnv)

## Path input
path_input = "~/Dropbox/data/polynya_contours_NCAR_2023/"

## Path output data
path_output = "~/Desktop/WHOI/Data/polynyas_contours/"

## Path output figure
path_fig = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")

#==================================================================
# 1) a. SSMI 
#==================================================================

file =  paste0(path_input, "ssmi_cdr_typical_polynya_mask_85%thresh")

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

nc_typical_polynyas <- nc_open(paste0(file, ".nc")) # satellite data
lon <- ncvar_get(nc_typical_polynyas, varid = "TLONG_sub")[,1]
lat <- ncvar_get(nc_typical_polynyas, varid = "TLAT_sub")[1,]
polynya_typical_all <-
  ncvar_get(nc_typical_polynyas, "polynya_typical_all") # code = 0 (other) or 1 (polynya)
polynya_typical_winter <-
  ncvar_get(nc_typical_polynyas, "polynya_typical_winter") # code = 0 (other) or 1 (polynya)

## Grid resolution
reso = get_grid_res(lon, lat)
print_grid_res(reso)

## Rearrange matrix to lon 0 -> 360 centered on 180
new_polynya <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_all)

file_pol_360 = paste0(path_output, "SSMI_polynya_typ_0_360")
saveRDS(new_polynya, file_pol_360)

## Rearrange matrix to lon 0 -> 360 centered on 180
new_polynya_w <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_winter)

file_pol_360_w = paste0(path_output, "WINTER_SSMI_polynya_typ_0_360")
saveRDS(new_polynya_w, file_pol_360_w)

#==================================================================
# 1) b. CESM
#==================================================================

file =  paste0(path_input, "jra55_typical_polynya_mask_hi_0.4mthresh")

nc_typical_polynyas <- nc_open(paste0(file, ".nc")) # satellite data
lon <- ncvar_get(nc_typical_polynyas, varid = "TLONG_sub")[,1]
lat <- ncvar_get(nc_typical_polynyas, varid = "TLAT_sub")[1,]
polynya_typical_all <-
  ncvar_get(nc_typical_polynyas, "polynya_typical_all") # code = 0 (other) or 1 (polynya)
polynya_typical_winter <-
  ncvar_get(nc_typical_polynyas, "polynya_typical_winter") # code = 0 (other) or 1 (polynya)

## Grid resolution
reso = get_grid_res(lon, lat)
print_grid_res(reso)

## Rearrange matrix to lon 0 -> 360 centered on 180
new_polynya <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_all)

file_pol_360 = paste0(path_output, "CESM_polynya_typ_0_360")
saveRDS(new_polynya, file_pol_360)

## Rearrange matrix to lon 0 -> 360 centered on 180
new_polynya_w <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_winter)

file_pol_360_w = paste0(path_output, "WINTER_CESM_polynya_typ_0_360")
saveRDS(new_polynya_w, file_pol_360_w)

#==================================================================
# 2) MAP POLYNYAS
#==================================================================

type = "CESM"
#type = "SSMI"

if (type == "CESM") {
  file_map = "CESM_typical_contours_polynyas.pdf"
  file_list_polynya = paste0(path_output, "CESM_polynya_typ_0_360")
} else {
  file_map = "SSMI_typical_contours_polynyas.pdf"
  file_list_polynya = paste0(path_output, "SSMI_polynya_typ_0_360")
}

list_polynya = readRDS(file_list_polynya)

lon = list_polynya$lon
lat = list_polynya$lat
z = list_polynya$z

title_map = paste0("Typical polynyas (", type, ") over 1979-2020 period \n(cells identified as polynyas > 10% of the time")

{
pdf(
  paste0(path_fig, file_map),
  height = 5,
  width = 12
)
print(image(lon, lat, z, 
            ylim = c(-80, -60), xlab = "Lon (°E)", ylab = "Lat (°N)"))
print(title(title_map))
dev.off()
}

#==================================================================
# 3) IDENTIFY POLYNYAS 
#==================================================================

types = c("CESM", "SSMI")

for (type in types) {
  
  if (type == "CESM") {
    file_list_polynya_id = paste0(path_output, "CESM_polynya_typ_id")
    file_list_polynya = paste0(path_output, "CESM_polynya_typ_0_360")
  } else {
    file_list_polynya_id = paste0(path_output, "SSMI_polynya_typ_id")
    file_list_polynya = paste0(path_output, "SSMI_polynya_typ_0_360")
  }
  
  list_polynya = readRDS(file_list_polynya)
  
  lon = list_polynya$lon
  lat = list_polynya$lat
  z = list_polynya$z
  
  z[which(is.na(z))] = 0
  z[which(z != 0)] = NA
  
  id_z <- identify_grouped_cells_matrix(z)
  
  list_pol_id = list("lon" = lon, "lat" = lat, "z" = id_z)
  
  saveRDS(list_pol_id, file_list_polynya_id)
  
}


#==================================================================
# 4) POLYNYA DATA TO DATAFRAME
#==================================================================

######### A BOUGER AU DEBUT -> QUE RASTER ET PAS MATRIX
types = c("CESM", "SSMI")

for (type in types) {
  
  if (type == "CESM") {
    #file_list_polynya_id = paste0(path_output, "CESM_polynya_typ_id")
    file_list_polynya_id = paste0(path_output, "WINTER_CESM_polynya_typ_0_360")
    #file_raster_polynya = paste0(path_output, "CESM_polynya_id_raster")
    file_raster_polynya = paste0(path_output, "WINTER_CESM_polynya_raster")
  } else {
    # file_list_polynya_id = paste0(path_output, "SSMI_polynya_typ_id")
    file_list_polynya_id = paste0(path_output, "WINTER_SSMI_polynya_typ_0_360")
    # file_raster_polynya = paste0(path_output, "SSMI_polynya_id_raster")
    file_raster_polynya = paste0(path_output, "WINTER_SSMI_polynya_raster")
  }
  
  list_polynya = readRDS(file_list_polynya_id)
  
  lon = list_polynya$lon
  lat = list_polynya$lat
  z = list_polynya$z
  
  r <- matrix_to_raster(z)
  plot(r)
  ext <- extent(min(lon), max(lon), min(lat), max(lat))
  extent(r) <- ext
  r <- setExtent(r, ext, keepres=TRUE)
  plot(r)
  
  writeRaster(r, file_raster_polynya, overwrite = T)
}
###########################

types = c("CESM", "SSMI")

for (type in types) {
  
  if (type == "CESM") {
    # file_df_polynya_id = paste0(path_output, "CESM_df_pol_id")
    file_df_polynya_id = paste0(path_output, "WINTER_CESM_df_pol")
    #file_raster_polynya = paste0(path_output, "CESM_polynya_id_raster")
    file_raster_polynya = paste0(path_output, "WINTER_CESM_polynya_raster")
  } else {
    # file_df_polynya_id = paste0(path_output, "SSMI_df_pol_id")
    file_df_polynya_id = paste0(path_output, "WINTER_SSMI_df_pol")
    # file_raster_polynya = paste0(path_output, "SSMI_polynya_id_raster")
    file_raster_polynya = paste0(path_output, "WINTER_SSMI_polynya_raster")
  }
  
  r <- raster(file_raster_polynya)
  
  df <- as.data.frame(r, xy = TRUE) %>% 
    na.omit()
  
  colnames(df) = c("lon", "lat", "poly_id")
  head(df)
  
  saveRDS(df, file_df_polynya_id)
  
}
## End script
rm(list=ls())
