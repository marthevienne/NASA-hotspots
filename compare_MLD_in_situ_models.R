## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-22
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
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")

## Dives with id_ctd
sel_dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(zone %in% c("shelf", "slope") & !is.na(id_ctd)) %>%
  filter(pol %in% c(selection)) %>%
  mutate(year = year(DE_DATE),
         month = month(DE_DATE)) %>%
  filter(year <= 2021 & month <= 8) %>%
  select(c(REF, NUM, id_ctd))

## In situ mixed layer depth
insitu <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
  filter(id_ctd %in% sel_dives$id_ctd) %>%
  mutate(month = month(time),
         year = year(time)) %>%
  select(c(month, year, MLD, interpLon, interpLat)) %>%
  mutate(layer = paste0(month.name[month], ".", year, ".1"))

loc = cbind(insitu$interpLon, insitu$interpLat)

# insitu_mean <- insitu %>%
#   group_by(month, year) %>%
#   reframe(mean = mean(MLD, na.rm = T))

## MLD raster
model <- brick("CESM_data/CESM-JRA-002branch-2004-2021.monthly.HMXL_DR_polynya.tiff")
names(model) <- lyr_names
range(values(model), na.rm = T)

mld <- NULL
for (lyr in names(model)[109:121]) {
  print(lyr)
  insitu_lyr <- insitu %>%
    filter(layer == lyr)
  
  if (nrow(insitu_lyr) > 0) {
    loc = cbind(insitu_lyr$interpLon, insitu_lyr$interpLat)
    
    lyr_model <- which(names(model) == lyr)
    z <- model[[lyr_model]]
    
    insitu_lyr$mld_model = extract(z, loc)/100
    
    mld <- rbind(mld, insitu_lyr)
    
    print(plot(z/100, main = lyr))
    print(points(loc, cex = .1))
    
  }
}

mld %>%
  na.omit() %>%
  count() / nrow(insitu) * 100
hist(mld$MLD - mld$mld_model, breaks = 100)

plot(z)
points(loc, cex = .1)


## End script
rm(list=ls())
