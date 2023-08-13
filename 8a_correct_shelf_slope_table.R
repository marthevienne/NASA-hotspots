##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-08
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
setwd("~/Desktop/WHOI/Data/bathy_data/")
## ---------------------------
## Library
library(raster)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/zonal_transect_bathy.R")
source("~/Desktop/WHOI/Codes/find_lat_end_slope.R")
## ---------------------------

table <- read.csv("shelf_slope_158.txt", sep = ",") #___value 1 <- end of shelf & value 2 <- end of slope
table$lon <- seq(-1, 157.5, 0.5)
colnames(table) <- c("lat_end_shelf", "bathy_end_shelf", "lat_end_slope", "bath_end_slope", "lon")

## Bathymetry data
bathy <- raster("RES_0.0041_BATHY_sub_ice_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
bathy
ext <- extent(bathy)

lon <- table$lon
transects <- zonal_transect_bathy(bathy, lon)

## Correct end of slope

cont_slope <- transects %>%
  filter(bathymetry <= -3500) %>%
  group_by(lon) %>%
  reframe("end_slope" = lat[1])

lon_df = subset(table, select = lon)
cont_slope <- lon_df %>% left_join(cont_slope, by = c("lon")) #___join table in case we don't attain -3500 m bathy in some longitudes
NAs_slope <- cont_slope %>% filter(is.na(end_slope))

## Treatment of remaining lon
transect <- transects %>% filter(lon %in% NAs_slope$lon)

ggplot(transect) +
  geom_line(mapping = aes(x = lat, y = bathymetry), col = "black") +
  facet_grid(lon~.) +
  geom_vline(aes(xintercept = lat[which(bathymetry == min(bathymetry, na.rm = T))]))+
  theme_light()

cont_slope_NA <- transect %>%
  filter(bathymetry <= -3380) %>%
  group_by(lon) %>%
  reframe("end_slope" = lat[1])

cont_slope$end_slope[cont_slope$lon %in% cont_slope_NA$lon] = cont_slope_NA$end_slope #___add end slope for bathy always above -3500 m

cont_slope$end_shelf = table$lat_end_shelf #___add latitude of end of continental shelf

cont_slope %>% filter(end_slope < end_shelf) #___anomaly

## Continent
bbox <- ext(-5, 170, -75, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)

p1 <- ggplot(cont_slope) +
  geom_line(aes(x = lon, y = end_slope), col = "darkgrey", linewidth = .3) +
  geom_line(aes(x = lon, y = end_shelf), col = "black", linewidth = .3) +
  geom_spatvector(data = wm) +
  theme_bw()

ggsave(plot = p1, "shelf_slope_3500.png", width = 30, height = 10, units = "cm", dpi = 200)

plot(table$lon, table$bath_end_slope, type = "l")

p2 <- ggplot(cont_slope) +
  geom_line(aes(x = lon, y = table$lat_end_slope), col = "darkgrey", linewidth = .3) +
  geom_line(aes(x = lon, y = end_shelf), col = "black", linewidth = .3) +
  geom_spatvector(data = wm) +
  theme_bw()

ggsave(plot = p2, "shelf_slope_Lucie.png", width = 30, height = 10, units = "cm", dpi = 200)

## Save regions limtis
write.csv(cont_slope, "shelf_slope_157.5_Marthe.txt", row.names = F)

## End script
rm(list=ls())