## ---------------------------
##
## Script name: define_MLD
##
## Purpose of script: Compute mixed layer depth following Boyer Montégut et al. (2004) on potential density anomaly gradient
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
setwd("~/Desktop/WHOI/Data/")
## ---------------------------
## Library
library(oce)
library(dplyr)
library(lubridate)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/compute_mld.R")
## ---------------------------

## CTD ids
id_ctd_dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  # mutate(month = month(DE_DATE),
  #        year = year(DE_DATE)) %>%
  #filter(pol > 0 & month <= 8 & year <= 2021) %>%
  pull(id_ctd) %>%
  na.omit() %>%
  unique()


## CTD stations
stations <- readRDS("ctd_data/ctd_stations_table_north_bound_interp") %>%
  filter(id_ctd %in% id_ctd_dives)

lonLat <- stations %>%
  dplyr::select(c(id_ctd, interpLon, interpLat))

## CTD profiles in polynyas
ctd <- readRDS("ctd_data/ctd_profiles_table") %>%
  filter(id_ctd %in% id_ctd_dives) %>%
  left_join(lonLat, by = "id_ctd")

## Dataframe for computing
df <- ctd %>%
  dplyr::select(c(id = id_ctd, depth, SP = psal, t = temp, lon = interpLon, lat = interpLat))

## Calculate MLD
df_mld <- compute_mld(df, eos = "gsw")
summary(df_mld$MLD)
df_mld %>%
  ungroup() %>%
  filter(is.na(MLD)) %>%
  count()

## Add MLD to stations table
stations_MLD <- stations %>%
  left_join(df_mld, by = c("id_ctd" = "id"))

## Scatter plot of MLD versus day of the year
ggplot(stations_MLD, aes(x = yday(time), fill = id_ctd, col = REF)) + 
  geom_point(aes(y = -MLD), size = .2) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_viridis_d()

saveRDS(stations_MLD, "ctd_data/ctd_stations_table_north_bound_interp_MLD")

## MLD == NA ?
stations_na <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
  filter(is.na(MLD))

stations_na %>% count()
summary(stations_na$max_depth)
hist(stations_na$max_depth, breaks = 30)

#----> CTD not deep enough (med = 454.0 m)

## MLD
stations <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
  filter(!is.na(MLD))

stations %>% count()
summary(stations$max_depth)
hist(stations$max_depth, breaks = 30)

ggplot(stations) +
  geom_point(aes(x = max_depth, y = MLD))

## End script
rm(list=ls())
