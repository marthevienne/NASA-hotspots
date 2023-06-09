##
## Script name: 
##
## Purpose of script:
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
setwd("~/Desktop/WHOI/Data/bathy_data/")
## ---------------------------
## Library
library(rnaturalearth)
## ---------------------------

source("~/Desktop/WHOI/Codes/palettes/palette_bathy.R")

## Shelf and slope limits
limits_bathy <- read.csv("shelf_slope_Marthe.txt", sep = ",")

## NA dataframe ====================
lim_NA <- limits_bathy %>%
  select(c(lon, end_slope)) %>%
  rename(lat = end_slope) %>%
  mutate(lat = NA)

## Continental slope polygon =======
lim_start_slope <- limits_bathy %>%
  select(c(lon, end_shelf)) %>%
  rename(lat = end_shelf)

lim_end_slope <- limits_bathy %>%
  select(c(lon, end_slope)) %>%
  arrange_at("lon", desc) %>%
  rename(lat = end_slope)

cont_slope <- rbind(lim_start_slope, lim_end_slope)
cont_slope$zone <- "slope"

## Continental shelf polygon ========
lim_start_shelf <- lim_NA %>% 
  mutate(lat = -71) #___arbitrary limit on land

lim_end_shelf <- lim_start_slope %>%
  arrange_at("lon", desc)

cont_shelf <- rbind(lim_start_shelf, lim_end_shelf)
cont_shelf$zone <- "shelf"

## Open-ocean polygon ===============
lim_end_oo <- lim_NA %>%
  mutate(lat = - 50)

open_ocean <- rbind(lim_end_slope, lim_end_oo)
open_ocean$zone = "open-ocean"

## Map polygons
ggplot() +
  geom_polygon(data = open_ocean, aes(x = lon, y = lat), fill = NA, col = col_d3, linewidth = .3) +
  geom_polygon(data = cont_slope, aes(x = lon, y = lat), fill = NA, col = col_d2, linewidth = .3) +
  geom_polygon(data = cont_shelf, aes(x = lon, y = lat), fill = NA, col = col_d1, linewidth = .3) +
  theme_classic()

## Compile polygons points in single file
compile_polygons <- rbind(open_ocean, cont_slope, cont_shelf)

## Save as CSV
write.csv(compile_polygons, "bathy_polygons.csv", row.names = F)

## End script
rm(list=ls())
