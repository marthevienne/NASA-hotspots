##
## Script name: assign_monthly_polynya_to_dives
##
## Purpose of script: Assign polynya id to dives: polynyas are defined by monthly polygons (01/2004 - 12/2019). 
##                    Polynyas can have multiple contours.
##                    For years without contours (after 2019), the biggest contour encountered during a given month on the period covered
##                    is taken as the default contour.
##                    
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-13
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
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/useful_functions/assign_polygon_to_points.R")
## ---------------------------


## Import dive data
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_6")

## Import polynya polygons
poly <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")
poly_big <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/biggest_contours_per_month/OBS_big_polynyas_contours_df.csv")

range(poly$year)
range(year(dives$DE_DATE))

df_pts <- dives %>%
  select(c(REF, NUM, interpLon, interpLat, DE_DATE)) %>%
  rename(lon = interpLon, lat = interpLat) %>%
  mutate(month = month.name[month(DE_DATE)],
         year = year(DE_DATE)) #___prepare pts df needed for "assign_polygon_to_points()"

poly <- poly %>%
  mutate(new_ID = paste0(ID, ".", num)) #___take into account multiple contours for one polynya

poly_big <- poly_big %>%
  mutate(new_ID = paste0(ID, ".", num)) #___take into account multiple contours for one polynya


years <- unique(df_pts$year)
split_polygons = "new_ID"
dives_id <- NULL

for(yr in years) {
  
  for (m in month.name) {
    
    dives_m <- df_pts %>%
      filter(month == m & year == yr) #___dives occuring at mm/YYYY
    
    if (nrow(dives_m) > 0) {
    
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
    
      dives_id_m <- assign_polygon_to_points(dives_m, poly_m, split_polygons, multiple = T) %>%
        select(c(REF, NUM, pol = id))
      
      dives_id <- rbind(dives_id, dives_id_m)
    }
  }
}

dives_id$pol[is.na(dives_id$pol)] = 0

new_dives <- dives %>% left_join(dives_id, by = c("REF", "NUM"))

## Save dives table
saveRDS(new_dives, "behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_7")

## End script
rm(list=ls())
