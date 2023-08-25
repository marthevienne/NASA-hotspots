## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-17
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
library(lubridate)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")

## Subset of dives
sel_dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(zone %in% c("shelf", "slope") & !is.na(id_ctd)) %>%
  filter(pol %in% c(0, selection)) %>%
  mutate(year = year(DE_DATE),
         month = month(DE_DATE)) %>%
  filter(year <= 2021 & month <= 8) %>%
  dplyr::select(c(REF, NUM))

#------------------------------------------------------------------
# METADATA TABLE
#------------------------------------------------------------------
meta <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  dplyr::select(c(REF, NUM, DE_DATE, pol, id_ctd, zone, dive_mode))

## Add month and year
meta <- meta %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE)) %>%
  dplyr::select(!DE_DATE) %>%

## Add season
  mutate(season = case_when(month <= 2 ~ "Summer",
                            month <= 5 ~ "Autumn",
                            month <= 8 ~ "Winter",
                            .default = NA)) %>%

## Inside/outside polynya
  mutate(inside = case_when(pol == 0 ~ "outside",
                            .default = "inside"))

## Filter meta table with selected dives
meta <- sel_dives %>%
  left_join(meta, by = c("REF", "NUM"))


## Assign dive number 
meta$id <- seq(1, nrow(meta))

df_id <- meta %>%
  dplyr::select(c(REF, NUM, id))
  
#------------------------------------------------------------------
# BEHAVIOUR TABLE (all dives)
#------------------------------------------------------------------
behav <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  dplyr::select(c(REF, NUM, MAX_DEP, DIVE_DUR, BOTT_TIME, hunting_time, SPEED_DESC, SPEED_ASC))

## Filter table with selected dives
behav <- sel_dives %>%
  left_join(behav, by = c("REF", "NUM"))

behav <- behav %>%
  left_join(df_id, by = c("REF", "NUM"))

hunt <- readRDS("behavioural_data/hunting_time_segments_longest_domWM_TS") %>%
  dplyr::select(c(REF, NUM, mean_depth_hs = mean_depth))

behav_final <- behav %>%
  left_join(hunt, by = c("REF", "NUM")) %>%
  dplyr::select(!c(REF, NUM))

#------------------------------------------------------------------
# ENVIRONNMENT TABLE (all dives)
#------------------------------------------------------------------
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  dplyr::select(REF, NUM, MAX_DEP, bathy, id_ctd, lon = interpLon, lat = interpLat) %>%
  mutate(bathy = case_when(-bathy < MAX_DEP ~ -MAX_DEP,
                           .default = bathy))

## Filter table with selected dives
dives <- sel_dives %>%
  left_join(dives, by = c("REF", "NUM"))

mld <- readRDS("ctd_data/ctd_stations_table_polynyas_MLD") %>%
  dplyr::select(c(id_ctd, MLD))

ctd <- readRDS("behavioural_data/hunting_time_segments_longest_domWM_TS") %>%
  dplyr::select(!c(perc, mean_depth))

## Final table
env <- dives %>%
  #left_join(mld, by = "id_ctd") %>%
  left_join(ctd, by = c("REF", "NUM")) %>%
  na.omit()

env <- env %>%
  left_join(df_id, by = c("REF", "NUM")) %>%
  dplyr::select(!c(REF, NUM, id_ctd, MAX_DEP)) %>%
  relocate(id)

#------------------------------------------------------------------
# Tidy table 
#------------------------------------------------------------------

## Keep only dives with CTD data (env table)
keep_dives_env <- env %>%
  pull(id)

keep_dives_behav <- behav %>%
  na.omit() %>%
  pull(id)
  
keep_dives <- intersect(keep_dives_env, keep_dives_behav)  

## Meta
meta_final <- meta %>%
  filter(id %in% keep_dives) %>%
  relocate(id)

## Behaviour
behav_final <- behav_final %>%
  filter(id %in% keep_dives) %>%
  relocate(id)

## Environmental
env_final <- env %>%
  filter(id %in% keep_dives) %>%
  relocate(id) %>%
  na.omit()



## Save tables
write.csv(meta_final, "~/Desktop/NASA-hotspots/NASA_CAP_Sara/meta.csv", )
write.csv(env_final, "~/Desktop/NASA-hotspots/NASA_CAP_Sara/varenv.csv")
write.csv(behav_final, "~/Desktop/NASA-hotspots/NASA_CAP_Sara/varbehav.csv")








source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")

##==== Behavioral data at the foraging segment scale
## At the dive scale
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0) %>%
  filter(pol %in% selection) %>%
  mutate(year = year(DE_DATE),
         month = month(DE_DATE)) %>%
  filter(year <= 2021 & month <= 8) %>%
  dplyr::select(c(REF, NUM, MAX_DEP, DIVE_DUR, hunting_time, pol))

## At the foraging segment scale
fs <- readRDS("behavioural_data/hunting_time_segments_domWM")

Y <- fs %>%
  left_join(dives, by = c("REF", "NUM"), relationship = "many-to-many") %>%
  dplyr::select(c(num_fs, MAX_DEP, DIVE_DUR, hunting_time, min_depth, max_depth, pol))

##==== Environmental data at the dive scale
mld <- readRDS("ctd_data/ctd_stations_table_polynyas_MLD") %>%
  dplyr::select(c(id_ctd, MLD, season))

dives_wm <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9")  %>%
  filter(pol > 0) %>%
  filter(pol %in% selection) %>%
  mutate(year = year(DE_DATE),
         month = month(DE_DATE)) %>%
  filter(year <= 2021 & month <= 8) %>%
  left_join(mld, by = "id_ctd") %>%
  dplyr::select(c(REF, NUM, MLD, season, month))

wm <- fs %>%
  filter(num_fs %in% Y$num_fs) 

X <- wm %>%
  left_join(dives_wm, by = c("REF", "NUM")) %>%
  dplyr::select(c(num_fs, water_mass, MLD, season, month))

saveRDS(Y, "canonical_analysis/matrix_Y_CAP")
saveRDS(X, "canonical_analysis/matrix_X_CAP")

## End script
rm(list=ls())
