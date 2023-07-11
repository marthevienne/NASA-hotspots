## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-07
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
library(dplyr)
library(spatialrisk)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/select_ctd.R")
source("~/Desktop/NASA-hotspots/useful_functions/assign_ctd.R")
## ---------------------------

## Import data
ctd <- readRDS("ctd_stations_table_north_bound_interp") #___CTD stations table
dives <- readRDS("dive_metrics_V8") #___dives tables

#------------------------------------------------------------------
# Check if every seal in CTD table is associated to a seal in dives table
#------------------------------------------------------------------

ref_ctd <- ctd %>%
  pull(REF) %>%
  unique()

ref_dives <- dives %>%
  pull(REF) %>%
  unique()

which(!ref_ctd %in% ref_dives) #___supposed to be == integer(0)

#------------------------------------------------------------------
# Assign CTD stations to dives
#------------------------------------------------------------------

max_dist <- 5000 #___(m) maximal distance between CTD and dive
seals <- ref_dives
tab_ctd_tot <- NULL #___df with result from selection on distance and timing between dive and CTD station

for (seal in seals[1:50]) {
  print(seal)
  
  ctd_seal <- ctd %>%
    filter(REF == seal) #___ctd associated to one seal
  
  dives_seal <- dives %>%
    filter(REF == seal) #___dives associated to one seal
  
  # for (i in 1:nrow(dives_seal)) {
  #   tab <- select_ctd(x = dives_seal[i,], data = ctd_seal, radius = max_dist)
  #   dives_ctd <- rbind(dives_ctd, tab)
  # }
  
  tab_ctd_seal <- assign_ctd(ctd_seal, dives_seal, max_dist)
  tab_ctd_tot <- rbind(tab_ctd_tot, tab_ctd_seal)
}

## For each dive, keep the closest ctd station in time (<= 12h)
tab_ctd_filt <- tab_ctd_tot %>%
    group_by(REF, NUM) %>%
    arrange(dist_t) %>%
    filter(row_number() == 1) %>%
    filter(dist_t <= 720) %>% #___min (12h)
    filter(max_depth_ctd >= MAX_DEP)

dives_sample <- dives %>%
  filter(REF %in% seals[1:50])

nrow(tab_ctd_filt) / nrow(dives_sample) * 100 #___fraction of dives with a ctd station

## Count the number of ctd stations attributed to each dive 
counts <- tab_ctd_tot %>%
  group_by(REF, NUM) %>%
  summarise(n_ctd = n())

seal_test <- tab_ctd_tot %>%
  filter(REF == "ct164-189-BAT-20" & NUM == 3895)


# full_tab <- tab_ctd_filt %>% 
#   left_join(dives, by = c("REF", "NUM")) %>%
#   select(c(REF, NUM, station, interpLon, interpLat))
# 
# dives_ctd <- dives %>% 
#   select(c(REF, NUM)) %>%
#   left_join(dives_ctd, by = c("REF", "NUM"))

# dives_ctd %>%
#   filter(is.na(station)) %>%
#   summarise(n = n()) / nrow(dives_ctd) * 100 #___fraction of dives with no ctd profile

# ## Distance between dives ##############
# dist_deg <- sqrt(diff(dives$interpLat)^2 + diff(dives$interpLon)^2)
# mean_lat <- mean(dives$interpLat)
# 
# library(okara)
# dist_km <- d2km(dist_deg, base.latitude = mean_lat)
# r <- dist_km/2
# summary(r)
# mean(r)
# hist(r, 500)
# hist(log(r), 30)
# 
# # Log-normal distribution
# esp <- exp(mean(r) + (sd(r)^2)/2)
# var <- (exp(sd(r)^2) - 1) * exp(2 * mean(r) + sd(r)^2)
# log_r <- log(r)
# 
# IC_min <- mean(log_r) - 1.96 * (sd(log_r) / sqrt(nrow(dives)))
# IC_max <- mean(log_r) + 1.96 * (sd(log_r) / sqrt(nrow(dives)))
# 
# exp(IC_min)
# exp(IC_max)
# 
# hist(log(dist_km/2), breaks = 30)
# summary(log(dist_km/2))
# sd(log(dist_km/2))
# #########################################
# 
# ## Distance between ctd #################
# dist_deg <- sqrt(diff(ctd$interpLat)^2 + diff(ctd$interpLon)^2)
# mean_lat <- mean(ctd$interpLat)
# 
# dist_km <- d2km(dist_deg, base.latitude = mean_lat)
# r <- dist_km/2
# summary(r)
# mean(r)
# sd(r)
# hist(r, 500)

## Condition on distance:
# Q3 (0.75)
##########################################

## End script
rm(list=ls())
