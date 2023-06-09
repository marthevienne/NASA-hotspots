##
## Script name: 
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
## ---------------------------

table <- read.csv("shelf_slope_158.txt", sep = ",")
table$lon <- seq(-0.5, 158, 0.5)
# value 1 <- end of shelf
# value 2 <- end of slope
colnames(table) <- c("lat_end_shelf", "bathy_end_shelf", "lat_end_slope", "bath_end_slope", "lon")

## Bathymetry data
bathy <- raster("RES_0.0041_BATHY_sub_ice_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
bathy

lon <- table$lon
points_lon <- rep(lon, each = bathy@nrows) #nrow in show(bathy)
by = (ext@ymax - ext@ymin) / (bathy@nrows - 1)
lat <- seq(ext@ymin, ext@ymax, by = abs(by))
points_lat <- rep(lat, length(lon))

transects <- as.data.frame(cbind(points_lon, points_lat))
depth <- extract(bathy, transects)
transects$depth <- depth

colnames(transects) = c("lon", "lat", "bathymetry")

## Correct end of slope
end_of_slope <- transects %>%
  filter(depth >= -3500) %>%
  group_by(lon) %>%
  reframe("depth_end_slope" = min(bathymetry, na.rm = T))

cont_slope <- end_of_slope %>%
  group_by(lon) %>%
  left_join(transects, by = c('lon', "depth_end_slope" = "bathymetry")) %>%
  filter(row_number() == 1) %>%
  rename("end_slope" = "lat")# %>%
  select(c(lon, end_slope))

cont_slope$end_shelf = table$lat_end_shelf

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
plot(cont_slope$lon, cont_slope$depth_end_slope, type = "l")

p2 <- ggplot(cont_slope) +
  geom_line(aes(x = lon, y = table$lat_end_slope), col = "darkgrey", linewidth = .3) +
  geom_line(aes(x = lon, y = end_shelf), col = "black", linewidth = .3) +
  geom_spatvector(data = wm) +
  theme_bw()

ggsave(plot = p2, "shelf_slope_Lucie.png", width = 30, height = 10, units = "cm", dpi = 200)

lines(cont_slope$lon, cont_slope$end_shelf, type = "l")

## End script
rm(list=ls())