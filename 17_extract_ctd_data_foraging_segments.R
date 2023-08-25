## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-16
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

## Polynyas of interest
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")

## Selected dives
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol %in% c(0, selection)) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE)) %>%
  filter(month <= 8 & year <= 2021) %>%
  dplyr::select(c(REF, NUM, id_ctd)) %>%
  na.omit()

## Foraging segments
fs <- readRDS("behavioural_data/hunting_time_segments")
str(fs)

## Number of hunting segments per dive
count_fs <- fs %>%
  group_by(REF, NUM) %>%
  count()

hist(count_fs$n)

## Keep longest segment for each dive
fs_long <- fs %>%
  group_by(REF, NUM) %>%
  arrange_at("Ddiff", .by_group = T, desc) %>%
  filter(row_number() == 1)

fs_long <- fs_long %>%
  group_by(REF, NUM) %>%
  reframe(mean_depth = mean_depth_HS,
          min_depth = mean_depth_HS - abs(Ddiff/2),
          max_depth = mean_depth_HS + abs(Ddiff/2))

fs_long$num_fs = seq(1, nrow(fs_long))

## For each CTD, extract theta, SP and WM for each foraging segment
ctd <- readRDS("ctd_data/gamma_n_data/ctd_profiles_table_WM") %>%
  filter(id_ctd %in% dives$id_ctd) %>%
  dplyr::select(c(id_ctd, depth, psal, temp, potTemp, water_mass))

cut_ctd <- fs_long %>%
  left_join(dives, by = c("REF", "NUM")) %>%
  dplyr::select(c(REF, NUM, min_depth, max_depth, mean_depth, id_ctd)) %>%
  filter(!is.na(id_ctd))

ctd_fs <- ctd %>%
  left_join(cut_ctd, by = "id_ctd", relationship = "many-to-many")

ctd_prof_fs <- ctd_fs %>%
  group_by(REF, NUM) %>%
  filter(between(depth, min_depth, max_depth)) %>%
  dplyr::select(c(REF, NUM, depth, psal, temp, potTemp, water_mass, mean_depth))


#------------------------------------------------------------------
# T/S
#------------------------------------------------------------------

ts_summ <- ctd_prof_fs %>%
  group_by(REF, NUM) %>%
  reframe(minS = min(psal), maxS = max(psal), meanS = mean(psal), 
          minT = min(temp), maxT = max(temp), meanT = mean(temp))

#------------------------------------------------------------------
# Dominant water mass per foraging segment
#------------------------------------------------------------------

fs_wm <- ctd_prof_fs %>%
  group_by(REF, NUM) %>%
  mutate(n = n()) %>%
  group_by(REF, NUM, water_mass) %>%
  reframe(perc = n()/n * 100) %>%
  distinct() %>%
  ungroup()

fs_wm <- fs_wm %>%
  group_by(REF, NUM) %>%
  arrange_at(.by_group = T, "perc", desc) %>%
  filter(row_number() == 1)

## Add min and max depth of fs
fs_depth <- fs_long %>%
  dplyr::select(REF, NUM, mean_depth)

fs_wm <- fs_wm %>%
  left_join(fs_depth, by = c("REF", "NUM"))

fs_wm %>%
  ungroup() %>%
  count()

summary(fs_wm$perc)

#------------------------------------------------------------------
# Combine T/S and WM
#------------------------------------------------------------------

fs_long_summ <- fs_wm %>%
  left_join(ts_summ, by = c("REF", "NUM"))

saveRDS(fs_long_summ, "behavioural_data/hunting_time_segments_longest_domWM_TS")

#------------------------------------------------------------------
# Foraging behaviour: Dominant water masses
#------------------------------------------------------------------

ctd_fs %>%
  group_by(water_mass) %>%
  reframe(nobs = n()/nrow(ctd_fs) * 100)


  
## End script
rm(list=ls())
