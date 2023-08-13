##
## Script name: assign_monthly_polynya_to_ctd_stations
##
## Purpose of script: Assign polynya id to ctd stations: polynyas are defined by monthly polygons (01/2004 - 12/2019). 
##                    Polynyas can have multiple contours.
##                    For years without contours (after 2019), the biggest contour encountered during a given month on the period covered
##                    is taken as the default contour.
##                    
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-17
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
library(dplyr)
library(lubridate)
library(sp)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/useful_functions/assign_polygon_to_points.R")
## ---------------------------

## Import data
ctd <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table_north_bound_interp")

## Import polynya polygons
poly <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")
poly_big <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/biggest_contours_per_month/OBS_big_polynyas_contours_df.csv")

range(poly$year)
range(year(ctd$time))

df_pts <- ctd %>%
  select(c(REF, station, interpLon, interpLat, time)) %>%
  rename(lon = interpLon, lat = interpLat) %>%
  mutate(month = month.name[month(time)],
         year = year(time)) #___prepare pts df needed for "assign_polygon_to_points()"

poly <- poly %>%
  mutate(new_ID = paste0(ID, ".", num)) #___take into account multiple contours for one polynya

poly_big <- poly_big %>%
  mutate(new_ID = paste0(ID, ".", num)) #___take into account multiple contours for one polynya


years <- unique(df_pts$year)
split_polygons = "new_ID"
loc_id <- NULL

for(yr in years) {
  
  for (m in month.name) {
    
    loc_m <- df_pts %>%
      filter(month == m & year == yr) #___dives occuring at mm/YYYY
    
    if (nrow(loc_m) > 0) {
      
      if (yr <= 2019) {
        poly_m <- poly %>%
          filter(month == m & year == yr)
      } else {
        poly_m <- poly_big %>%
          filter(month == m)
      }
      
      keep_poly <- poly_m %>%
        group_by(new_ID) %>%
        summarise(n = n()) %>%
        filter(n > 4) %>%
        pull(new_ID)
      
      poly_m <- poly_m %>% filter(new_ID %in% keep_poly)
      
      loc_id_m <- assign_polygon_to_points(loc_m, poly_m, split_polygons, multiple = T) %>%
        select(c(REF, station, pol = id))
      
      loc_id <- rbind(loc_id, loc_id_m)
    }
  }
}

loc_id$pol[is.na(loc_id$pol)] = 0

new_ctd <- ctd %>% left_join(loc_id, by = c("REF", "station"))

new_ctd %>%
  filter(pol > 0) %>%
  summarise(nobs = sum(max_depth)) #___total number of observations in polynyas

## Save table
saveRDS(new_ctd, "~/Desktop/WHOI/Data/output_data/ctd_stations_table_polynyas")

## End script
rm(list=ls())
