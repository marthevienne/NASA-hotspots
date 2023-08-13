## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-21
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
setwd("~/Desktop/WHOI/Data/")
## ---------------------------
## Library
library(dplyr)
library(lubridate)
library(raster)
library(sf)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/df_to_SpatialPolygons.R")
## ---------------------------

## Polynya polygons
polynya <- read.csv("polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv") %>%
  mutate(newID = paste0(month, '.', year,'.', ID, '.', num))

ids <- polynya %>%
  dplyr::select(newID) %>%
  distinct() %>%
  pull(newID)

## Coastal zones
zones <- read.csv("bathy_data/bathy_polygons.csv") %>%
  filter(zone == "shelf")

z <- df_to_SpatialPolygons(zones, split_polygons =  "zone")
projection(z) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
coords_z <- z@polygons[[1]]@Polygons[[1]]@coords

areas <- NULL
areas <- tibble(pol = NA, month = NA, year = NA, id = NA, 
                area = NA, area_shelf = NA)

j = 1
for (id in ids) {
  print(j)
  poly <- polynya %>%
    filter(newID == id)
  
  i = unique(poly$ID)
  m = unique(poly$month)
  yr = unique(poly$year)
  
  sp <- df_to_SpatialPolygons(poly, split_polygons = "num")
  projection(sp) <- projection(z)
  area <- as.numeric(sp@polygons[[1]]@Polygons[[1]]@area)
  
  if (area > 0) {

    ## Check if sp crosses z
    coords <- sp@polygons[[1]]@Polygons[[1]]@coords
    
    plot(sp)
    plot(z, add = T)
    
    test <- point.in.polygon(
      point.x = coords[, 1],
      point.y = coords[, 2],
      pol.x = coords_z[, 1],
      pol.y = coords_z[, 2]
    )
    
    if (length(test[test != 0]) != 0) {
      inters <- intersect(z, sp)
      area_shelf <- as.numeric(inters@polygons[[1]]@Polygons[[1]]@area)
    
      areas <- rbind(areas, c("pol" = i, "month" = m, "year" = yr, "id" = id, 
                              "area" = area, "area_shelf" = area_shelf))
    } else {
      areas <- rbind(areas, c("pol" = i, "month" = m, "year" = yr, "id" = id, 
                             "area" = area, "area_shelf" = 0))
    }
  }
  
  j = j + 1
}

areas <- areas %>%
  mutate(cov = area_shelf / area * 100)

areas$area = as.numeric(areas$area)
areas$area_shelf = as.numeric(areas$area_shelf)

areas_tot <- areas %>%
  group_by(pol, year, month) %>%
  reframe(tot_area = sum(area),
          area_shelf = sum(area_shelf),
          cov = area_shelf / tot_area * 100)
areas_tot$cov[areas_tot$cov > 100] = 100

saveRDS(areas_tot, "~/Desktop/WHOI/Data/polynyas_contours/OBS/coverage_shelf_all_polynyas")

ggplot() +
  geom_boxplot(data = areas_tot, aes(x = factor(pol), y = cov, col = factor(pol)))

## End script
rm(list=ls())
