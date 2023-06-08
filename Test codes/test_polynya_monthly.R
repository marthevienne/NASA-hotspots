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
# 1) a. Monthly polynyas
#==================================================================

file =  paste0(path_input, "SSMI.CDR.85%thresh.polynya2_sh.197901-202012")

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

# Get variables : lon, lat, time, 3D matrix (polynyas ids and types raster through time (1979-01))
nc_polynyas <- nc_open(paste0(file, ".nc")) # satellite data
lon <- ncvar_get(nc_polynyas, varid = "tlon1d")
lat <- ncvar_get(nc_polynyas, "tlat1d", verbose = F)
polynya_matrix_id <- ncvar_get(nc_polynyas, "polynya_ID")
polynya_matrix_type <- ncvar_get(nc_polynyas, "polynyas")
time = seq(as.Date("1979/1/1"), as.Date("2020/12/1"), "month")
polyID = ncvar_get(nc_polynyas, "polyID")

## Grid resolution
reso = get_grid_res(lon, lat)
print_grid_res(reso)

# ## Rearrange matrix to lon 0 -> 360 centered on 180
# new_polynya <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_all)
# 
# file_pol_360 = paste0(path_output, "SSMI_polynya_typ_0_360")
# saveRDS(new_polynya, file_pol_360)
# 
# ## Rearrange matrix to lon 0 -> 360 centered on 180
# new_polynya_w <- rearrange_spatial_matrix_360(lon, lat, polynya_typical_winter)
# 
# file_pol_360_w = paste0(path_output, "WINTER_SSMI_polynya_typ_0_360")
# saveRDS(new_polynya_w, file_pol_360_w)

#==================================================================
# POLYNYA DATA TO DATAFRAME
#==================================================================
image(polynya_matrix_type[,,1])

rotate1 = aperm(polynya_matrix_type, c(2,1,3))
rotate2 <- rotate1[nrow(rotate1):1,,]
rotate2[rotate2 == 0] = NA
rotate2[rotate2 == 2] = "polynya"

lyr_names = sapply(time, function(t) ifelse(month(t) < 10, 
                                            paste0("0",  month(t), "-", year(t)), 
                                            paste0(month(t), "-", year(t))))

toto = terra::rast(nrow = length(lat), ncol = length(lon), nl = length(time), 
                   names = lyr_names, vals = rotate2)

rotate1 = aperm(polynya_matrix_type, c(2,1,3))
rotate2 <- rotate1[nrow(rotate1):1,,]
rotate2[rotate2 == 0] = NA
rotate2[rotate2 == 2] = 1

# image(rotate2[,,1])


toto = terra::rast(nrow = length(lat), ncol = length(lon), nl = length(time), 
                   names = lyr_names, vals = rotate2)

# toto[toto == 1] <- "polynya"

terra::ext(toto) <- c(min(lon), max(lon), min(lat), max(lat))
plot(toto$`01-1979`)
# toto_proj <- project(toto, "EPSG:4326")

writeRaster(toto, "~/Desktop/WHOI/Data/polynyas_contours/NCAR_pol_monthly.tif", overwrite = T)

test = as.data.frame(toto)
test = as.data.frame(toto, row.names = NULL, optional = FALSE, xy=T, cells=FALSE, na.rm=NA)


ggplot() +
  geom_spatraster(data = toto) +
  coord_sf(crs = 3857) +
  facet_wrap(~lyr, ncol = 2, nrow = 5)+
  scale_fill_hypso_c()+
  theme_minimal()

layers_pol = names(toto)
# dives$lyr = paste0(month(dives$DE_DATE, label = T), " ", year(dives$DE_DATE))
dives$lyr = sapply(dives$DE_DATE, function(t) ifelse(month(t) < 10, 
                                            paste0("0",  month(t), "-", year(t)), 
                                            paste0(month(t), "-", year(t))))
head(dives$lyr)
month_with_dives = intersect(layers_pol, unique(dives$lyr))
month_wo_dives = layers_pol[which(!(layers_pol %in% month_with_dives))]

test = toto[[month_with_dives]]
dives_sample = subset(dives, lyr %in% month_with_dives)
names(test)
unique(dives_sample$lyr)

ggplot() +
  geom_spatraster(data = toto, aes(fill = `1979-1`)) +
  # You can use coord_sf
  scale_fill_hypso_c()

topo_EA <- raster("~/Desktop/WHOI/Data/bathy_data/HR_0.0041_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
topo_EA_0.41 <- raster("~/Desktop/WHOI/Data/bathy_data/LR_0.41_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
topo_EA_0.041 <- raster("~/Desktop/WHOI/Data/bathy_data/LR_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.grd")
topo_EA_0.083 <- raster("~/Desktop/WHOI/Data/bathy_data/LR_0.083_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")

df_0.41 <- as.data.frame(topo_EA_0.41, xy = TRUE)
head(df_0.41)

map0 <- ggplot()+
  geom_raster(data = topo_EA_0.083, 
              aes(x = long, y = lat, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
  labs(fill = "Bathymetry (m)")

map1 <- ggplot()+
  geom_raster(data = df_0.083, 
              aes(x = x, y = y, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
  labs(fill = "Bathymetry (m)")
  
library(ggpubr)
ens_maps <- ggarrange(map0, map1, nrow = 2, ncol = 1)
ggsave("~/Desktop/WHOI/Test figures/compare_df_VS_raster_bathy.png", height = 30, width = 60, units = c("cm"), dpi = 100)


map0 <- ggplot()+
  geom_raster(data = topo_EA, 
              aes(x = long, y = lat, fill = layer))+
  labs(fill = "Bathymetry (m)")

map1 <- ggplot()+
  geom_raster(data = topo_EA_0.041, 
              aes(x = long, y = lat, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
  labs(fill = "Bathymetry (m)")


names(toto)


p1 <- ggplot()+
  geom_raster(data = topo_EA, 
              aes(x = long, y = lat, fill = layer))+
  labs(fill = "Bathymetry (m)") +
  new_scale_fill()+
  geom_spatraster(data = toto) +
  #coord_sf(crs = 3857) +
  facet_wrap(~lyr, ncol = 2, nrow = 5)+
  scale_fill_hypso_c()+
  #coord_quickmap()+
  lims(x = c(-5, 170), y = c(-73, -58.5))


library(ggforce)

ggplot()+
  geom_spatraster(data = test) +
  geom_point(data = dives, mapping = aes(x = lon, y = lat), size = .2) +
  facet_wrap_paginate(~lyr, ncol = 2, nrow = 6, page = i)+
  #coord_sf(crs = 3857) +
  #facet_wrap(~lyr, ncol = 2, nrow = 6)+
  scale_fill_hypso_c()+
  #coord_quickmap()+
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  theme_classic()

test_proj <- project(test, dest_proj)

cls <- data.frame(id = 1, cover = c("polynya"))
for (lyr in names(test)) {
  levels(test[lyr]) <- cls
}
is.factor(test)

cats(test, )

terra::addCats(test, cls, merge=FALSE, layer=0)
setCats(test)

test["2004"]
npages = length(unique(dives$lyr))/6
years = as.character(sort(unique(year(dives$DE_DATE))))
for(i in 1:npages){
  #png(sprintf("~/Desktop/WHOI/Data/%s_test_facet.png", i), height = 40, width = 35, units = "cm", res = 150)
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya contours/by_month_w:_dives/map_pol_dives_%i.png", i), height = 40, width = 35, units = "cm", res = 150)
  print(ggplot()+
          # geom_raster(data = df_0.083,
          #             aes(x = x, y = y, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
          # labs(fill = "Bathymetry (m)")+
          #new_scale_fill()+
          geom_spatraster(data = test) +
          geom_spatvector(data = wm)+
          geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "black", size = .01, alpha = .6) +
          facet_wrap_paginate(~lyr, ncol = 1, nrow = 6, page = i)+
          #coord_sf(crs = 4326) +
          #facet_wrap(~lyr, ncol = 2, nrow = 6)+
          scale_fill_viridis_c(option = "D", na.value = NA, direction = -1)+
          #coord_quickmap()+
          lims(x = c(-5, 170), y = c(-73, -58.5))+
          theme_light())
  dev.off()
}

years = as.character(sort(unique(year(dives$DE_DATE))))
for(yr in years){
  pol_yr = test[yr]
  dives_yr = subset(dives, as.character(year(DE_DATE)) == yr)
  dives_yr = dives_yr[order(dives_yr$DE_DATE), ]
  nrow = length(unique(dives_yr$lyr))
  #png(sprintf("~/Desktop/WHOI/Data/%s_test_facet.png", i), height = 40, width = 35, units = "cm", res = 150)
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya contours/by_month_w:_dives/%s_map_pol_dives.png", yr), height = nrow*6, width = 30, units = "cm", res = 200)
  print(ggplot()+
          # geom_raster(data = df_0.083,
          #             aes(x = x, y = y, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
          # labs(fill = "Bathymetry (m)")+
          #new_scale_fill()+
          geom_spatvector(data = wm)+
          geom_spatraster(data = pol_yr) +
          geom_point(data = dives_yr, mapping = aes(x = lon, y = lat), col = "black", size = .01, alpha = .6) +
          facet_wrap(~lyr, nrow = nrow, ncol = 1)+
          #coord_sf(crs = 4326) +
          #facet_wrap(~lyr, ncol = 2, nrow = 6)+
          scale_fill_viridis_c(option = "D", na.value = NA, direction = -1)+
          #coord_quickmap()+
          lims(x = c(-15, 170), y = c(-73, -58.5))+
          theme_light())
  dev.off()
}

for(yr in c("2021", "2022")){
  # pol_yr = test[yr]
  dives_yr = subset(dives, as.character(year(DE_DATE)) == yr)
  nrow = length(unique(dives_yr$lyr))
  #png(sprintf("~/Desktop/WHOI/Data/%s_test_facet.png", i), height = 40, width = 35, units = "cm", res = 150)
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya contours/by_month_w:_dives/%s_map_pol_dives.png", yr), height = nrow*6, width = 30, units = "cm", res = 200)
  print(ggplot()+
          # geom_raster(data = df_0.083,
          #             aes(x = x, y = y, fill = gebco_2023_n.58.0_s.74.0_w.5.0_e170.0))+
          # labs(fill = "Bathymetry (m)")+
          #new_scale_fill()+
          geom_spatvector(data = wm)+
          # geom_spatraster(data = pol_yr) +
          geom_point(data = dives_yr, mapping = aes(x = lon, y = lat), col = "black", size = .01, alpha = .6) +
          facet_wrap(~lyr, nrow = nrow, ncol = 1)+
          #coord_sf(crs = 4326) +
          #facet_wrap(~lyr, ncol = 2, nrow = 6)+
          scale_fill_viridis_c(option = "D", na.value = NA, direction = -1)+
          #coord_quickmap()+
          lims(x = c(-15, 170), y = c(-73, -58.5))+
          theme_light())
  dev.off()
}



for(i in 1:2){
  print(ggplot() +
  geom_spatraster(data = test, aes(fill = `2015-3`)) +
  coord_sf(crs = 3857) +
  # geom_point(data = dives_sample, mapping = aes(x = lon, y = lat), size = .1) +
  # facet_wrap_paginate(~lyr, ncol = 1, nrow = 3, page = i)+
  # You can use coord_sf
  scale_fill_hypso_c())#+
  #lims(x = c(-5, 170), y = c(-73, -58.5)))
}

# p1 <- ggplot()+
#   geom_tile(data = df, 
#             aes(x = x, y = y, fill = layer), 
#             inherit.aes = F) +
#   new_scale_fill()+
#   geom_spatraster(data = toto) +
#   #coord_sf(crs = 3857) +
#   facet_wrap(~lyr, ncol = 2, nrow = 2)+
#   scale_color_hypso_b()+
#   #coord_quickmap()+
#   lims(x = c(-5, 170), y = c(-73, -58.5))
 
ggsave("~/Desktop/WHOI/Data/polynyas_contours/map_test_SpatRaster.png", height = 50, width = 60, units = c("cm"), dpi = 100)













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
