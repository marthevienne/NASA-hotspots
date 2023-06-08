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
path_input = "~/Dropbox/data/polynya_contours_NCAR_2023/High res run/"

## Path output data
path_output = "~/Desktop/WHOI/Data/polynyas_contours/OBS/"

type = "HI"
#type = "AICE"
#==================================================================
# 
#==================================================================
if (type == "HI") {
  file =  paste0(path_input, "g.e21.GIAF.TL319_t13.5thCyc.ice.001.monthly_clim.coastal_polynyas.hi_0.1mthresh.polynya_sh.0291-0308.2004-2021")
  map.filename = "HI_polynyas_maps.pdf"
  path_fig = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/High res run/HI/")
} else {
  file =  paste0(path_input, "g.e21.GIAF.TL319_t13.5thCyc.ice.001.monthly_clim.coastal_polynyas.aice_15%thresh.polynya_sh.0291-0308.2004-2021")
  map.filename = "AICE_polynyas_maps.pdf"
  path_fig = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/High res run/AICE/")
}

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

nc_polynyas <- nc_open(paste0(file, ".nc")) # satellite data
lon <- ncvar_get(nc_polynyas, varid = "tlon1d")
lat <- ncvar_get(nc_polynyas, varid = "tlat1d")
lat_cor <- lat - 0.5
polynyas <-
  ncvar_get(nc_polynyas, "polynya_regions") # code = 0 (other) or 1 (polynya)

## Grid resolution
reso = get_grid_res(lon, lat)
print_grid_res(reso)

## To dataframe
all_months_df = NULL
for (i in 1:length(month.name)) {
  m = polynyas[,,i]
  r = transform_to_raster(file_name = paste0(file, ".nc"), 
                          lon_name = "tlon1d",
                          lat_name = "tlat1d",
                          var_name = "polynya_regions", 
                          nyears = 1, depth = FALSE, integral = F, ratio = F 
                          )
  r = matrix_to_raster(m)
  plot(r)
  ext <- extent(min(lon), # - reso[["lon_r"]]/2, 
                max(lon), # + reso[["lon_r"]]/2, 
                min(lat_cor), # - reso[["lat_r"]]/2, 
                max(lat_cor)) # + reso[["lat_r"]]/2)
  extent(r) <- ext
  
  r <- setExtent(r, ext, keepres = TRUE)
  plot(r)
  abline(h = -60)
  r[r != 1] = NA
  df <- as.data.frame(r, xy = TRUE) %>% 
    na.omit() %>%
    select(c("x", "y"))
  
  colnames(df) = c("lon", "lat")
  #head(df)
  df$month = rep(month.name[i], nrow(df))
  
  all_months_df = rbind(all_months_df, df)
}

saveRDS(all_months_df, paste0(path_output, type, "_HR_monthly_polynyas"))


## Map per month
dives = readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_V3")

dives$month_num = sapply(dives$DE_DATE, function(t) ifelse(month(t) < 10, 
                                                           paste0("0",  month(t)), 
                                                           paste0(month(t))))
dives$month = sapply(dives$DE_DATE, function(t) month.name[month(t)])
head(dives$month)


## Continent 
bbox <- ext(-5, 170, -80, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)

all_months_df$month = factor(all_months_df$month, levels = month.name)
dives$month = factor(dives$month, levels = month.name)


library(ggforce)
map_pol_dives <- 
  ggplot() +
  geom_spatvector(data = wm)+
  geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "darkgrey", size = .001, alpha = .2) +
  geom_tile(data = all_months_df, aes(x = lon, y = lat), fill = "pink", alpha = .8) +
  #scale_color_viridis_d()+
  facet_wrap_paginate(~month, nrow = 12, ncol = 1) +
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N")

ggsave(sprintf("~/Desktop/%s_test.png", type), height = 100, width = 60, units = c("cm"), dpi = 300)











plot(lon)
#image(lon, lat, polynyas[,,1])

dest_proj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +x_0=6000000 +y_0=6000000 +datum=WGS84 +units=m +no_defs"
bbox <- ext(-5, 170, -74, -60)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox) #|>
  #project(dest_proj)
plot(wm)
proj <- crsuggest::suggest_crs(r2)
proj$crs_proj4

# polynyas_OBS = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/Ester_polynyas_contours_bigger_df.csv")
# polynyas_OBS$month = factor(polynyas_OBS$month, levels = month.name)

polynyas_OBS_multi = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/Matlab/OBS_multiple_polynyas_contours_df.csv")

# polynyas_OBS_multi_m = subset(polynyas_OBS_multi, month == "January")
# polynyas_OBS_multi_m$new_ID = paste0(polynyas_OBS_multi_m$month,".", polynyas_OBS_multi_m$ID, ".", polynyas_OBS_multi_m$num)
# 
# ggplot() +
#   geom_polygon(data = polynyas_OBS_multi_m, aes(x = lon, y = lat, group = new_ID), col = "black", fill = NA)
# 
# 
# keep_num = polynyas_OBS_multi_m %>% count(new_ID) %>%
#   filter(n > 3)
# 
# keep_pol = subset(polynyas_OBS_multi_m, new_ID %in% keep_num$new_ID)
# toto <-  sfheaders::sf_polygon(
#   obj = keep_pol
#   , x = "lon"
#   , y = "lat"
#   , polygon_id = "new_ID"
# ) 
# 
# union <- st_union(toto)
# 
# ggplot() +
#   geom_sf(data = union, col = "black", fill = "black")


lat_cor <- lat - 0.5
{
  pdf(paste0(path_fig, map.filename),
      height = 4,
      width = 10)
  for (index in 1:length(month.name)) {
  #index = 1
    ## OBS polynyas
    polynyas_OBS_multi_m = subset(polynyas_OBS_multi, month == month.name[index])
    polynyas_OBS_multi_m$new_ID = paste0(polynyas_OBS_multi_m$month,".", polynyas_OBS_multi_m$ID, ".", polynyas_OBS_multi_m$num)
    keep_num = polynyas_OBS_multi_m %>% count(new_ID) %>%
      filter(n > 3)
    
    keep_pol = subset(polynyas_OBS_multi_m, new_ID %in% keep_num$new_ID)
    toto <-  sfheaders::sf_polygon(
      obj = keep_pol
      , x = "lon"
      , y = "lat"
      , polygon_id = "new_ID"
    ) 
    
    union <- st_union(toto) %>% st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

    #pol_month_OBS = subset(polynyas_OBS_multi, month == month.name[index])
    r = matrix_to_raster(polynyas[,,index])
    ext <- extent(min(lon) - reso[["lon_r"]]/2, 
                  max(lon) + reso[["lon_r"]]/2, 
                  min(lat_cor) - reso[["lat_r"]]/2, 
                  max(lat_cor) + reso[["lat_r"]]/2)
    extent(r) <- ext
    r <- setExtent(r, ext, keepres = TRUE)
    e <- as(extent(-5, 170, -72, -60), 'SpatialPolygons')
    crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crs(e) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    r2 <- crop(r, e)
    #plot(r2)
    
    r2_rast = rast(r2)
    crs(r2_rast) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    # r2_projected <- project(r2_rast, dest_proj) # Projette ta bathy dans le bon système
    # r2_projected

    map <- ggplot() +
      geom_spatraster(data = r2_rast) +
      geom_spatvector(data = wm) +
      #geom_polygon(data = pol_month_OBS, aes(x = lon, y = lat, group = interaction(ID, num)), col = NA, fill = "red", alpha = .8) +
      geom_sf(data = union, col = "red", alpha = .4)+
      # geom_spatvector(data = pts, aes(color = takuse)) + # Tes points+
      scale_fill_hypso_c(na.value = "NA", palette = "arctic") + # La palette
      theme_minimal()+
      ggtitle(month.name[index])+
      theme(legend.position = "none")
    
    print(map)
  }
  dev.off()
}

## Rearrange matrix to lon 0 -> 360 centered on 180
r = matrix_to_raster(polynyas[,,1])
ext <- extent(min(lon) - reso[["lon_r"]]/2, 
              max(lon) + reso[["lon_r"]]/2, 
              min(lat) - reso[["lat_r"]]/2, 
              max(lat) + reso[["lat_r"]]/2)
extent(r) <- ext
r <- setExtent(r, ext, keepres=TRUE)


library(terra)
library(tidyterra)
library(ggplot2)

e <- as(extent(-5, 170, -72, -60), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r2 <- crop(r, e)
plot(r2)

r2_rast = rast(r2)
crs(r2_rast) = "EPSG:4326"
plot(r2_rast)

# proj <- crsuggest::suggest_crs(r2_rast) # Fonction qui va te dire quel système de projection est le plus adapté à ta zone 
# 
# dest_proj <- "EPSG:3031" # Modifie en fonction de celui conseillé par la zone d'avant
# 
# r2_projected <- project(r2_rast, dest_proj) # Projette ta bathy dans le bon système
# r2_projected

ggplot() +
  geom_spatraster(data = r2_rast) + # La bathy
  # geom_spatvector(data = pts, aes(color = takuse)) + # Tes points+
  scale_fill_hypso_c(na.value = "white", palette = "arctic") + # La palette
  theme_minimal()


new_polynya <- rearrange_spatial_matrix_360(lon, lat, polynyas)

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
