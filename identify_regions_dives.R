##
## Script name: identify_regions_dives.R
##
## Purpose of script: Assign an oceanographic region to each dive (e.g. open-ocean, continental slope or continental shelf)
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-09
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
setwd("~/Desktop/WHOI/Data/output_data/")
## ---------------------------
## Library
library(sp)
library(rnaturalearth)
library(ggplot2)
## ---------------------------

source("~/Desktop/WHOI/Codes/assign_polygon_to_points.R")

## Dive data
dives <- readRDS("dive_metrics_V4")

## Bathymetric features polygons
bathy_feat <- read.csv("~/Desktop/WHOI/Data/bathy_data/bathy_polygons.csv")

# Prepare df of locations to assign a region (polygons)
df_pts <- dives %>%
  select(c(REF, NUM, interpLon, interpLat)) %>%
  rename(lon = interpLon, lat = interpLat)

df_pts_region <- assign_polygon_to_points(df_pts, bathy_feat, "zone")

df_pts_region <- df_pts_region %>%
  select(c(REF, NUM, id)) %>%
  rename(zone = id)

dives_region <- dives %>% left_join(df_pts_region, by = c('REF', 'NUM'), suffix = c("", ""))
head(dives_region)

## Save dives with oceanographic region
saveRDS(dives_region, "dive_metrics_V5")

#==================================================================
# CHECK ZONES ON MAP 
#==================================================================
rm(list=ls())

dives <- readRDS("dive_metrics_V5")

source("~/Desktop/WHOI/Codes/palettes/pal_bathy.R") #___palette bathy

## Continent
bbox <- ext(-5, 156, -75, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)
dives_sample <- dives[sample(seq(1:nrow(dives)), 40000), ]
limits_bathy <- read.csv("~/Desktop/WHOI/Data/bathy_data/shelf_slope_Marthe.txt", sep = ",")

pal <- pal_bathy(3)
regions <- ggplot() +
  geom_spatvector(data = wm) +
  geom_line(data = limits_bathy, aes(x = lon, y = end_shelf), col = "black", linewidth = .3) +
  geom_line(data = limits_bathy, aes(x = lon, y = end_slope), col = "black", linewidth = .3) +
  geom_point(data = dives_sample, aes(x = interpLon, y = interpLat, col = factor(zone)), size = .1) +
  scale_color_manual("Zones", values = pal, na.value = "darkgrey") +
  ylim(limits = c(-75, -60)) +
  xlim(limits = (range(dives$interpLon, na.rm = T))) +
  theme_minimal()

ggsave("~/Dropbox/data/outputs_Marthe_2023/shelf_slope_map.png", height = 10, width = 40, units = "cm", dpi = 200)

## End script
rm(list=ls())
