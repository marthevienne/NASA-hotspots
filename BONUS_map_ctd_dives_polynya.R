## ---------------------------
##
## Script name: 
##
## Purpose of script:
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
setwd("~/Desktop/WHOI/Data/output_data/")
## ---------------------------
## Library
library(ggforce)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Import data
ctd <- readRDS("ctd_stations_table_north_bound_interp")
dives <- readRDS("dive_metrics_V9_ctd")
polynya <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

## Map sample ctd station
ref = "ct96-25-13"
m = "July"

ctd_ind <- ctd %>%
  filter(REF == ref & month.name[month(time)] == m) %>%
  filter(row_number() < 10)

dives_ind <- dives %>%
  filter(REF == ref) %>%
  filter(as.numeric(DE_DATE) >= as.numeric(min(ctd_ind$time)) - 100 & as.numeric(DE_DATE) <= as.numeric(max(ctd_ind$time)) + 100)
  #filter(station == 586)

polynya_cont <- polynya %>%
  filter(ID %in% unique(dives_ind$pol) & month == m & year == unique(year(dives_ind$DE_DATE)))

ddeg <- 0.06
xlim = c(min(dives_ind$interpLon) - ddeg, max(dives_ind$interpLon) + ddeg)
ylim = c(min(dives_ind$interpLat) - ddeg, max(dives_ind$interpLat) + ddeg)
ggplot() +
  geom_polygon(data = polynya_cont, aes(x = lon, y = lat, group = interaction(num, month)), col = "black", fill = NA) +
  geom_point(data = ctd_ind, aes(x = interpLon, y = interpLat, col = factor(station)), size = 2) +
  geom_path(data = dives_ind, aes(x = interpLon, y = interpLat), col = "lightgrey", linewidth = .4) +
  geom_point(data = dives_ind, aes(x = interpLon, y = interpLat, col = factor(station)), size = 1, shape = 4) +
  geom_circle(data = ctd_ind, aes(x0 = interpLon, y0 = interpLat, r = 0.05, col = factor(station)), inherit.aes = F)+
  xlim(xlim) +
  ylim(ylim) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

ggsave(sprintf("~/Desktop/zoom_map_ctd_dives_%s_%s.png", ref, m), height = 10, width = 20, units = "cm", dpi = 300)

##############
ggplot() +
  geom_tile(data = ctd_ind, aes(x = time, y = "CTD", fill = factor(station)))+
  geom_tile(data = dives_ind, aes(x = DE_DATE, y = "dives", col = factor(station), fill = factor(station)))+
  geom_text(aes(x = dives_ind$DE_DATE, y = "dives", label = dives_ind$NUM), angle = 90)+
  geom_text(aes(x = ctd_ind$time, y = "CTD", label = ctd_ind$station))+
  xlab("")+
  ylab("")+
  theme_light()+
  theme(legend.position = "none")

tab_ctd_tot <- readRDS("dives_ctd_assigned_not_filt")

tab_ctd_station <- tab_ctd_tot %>%
  filter(REF == "ct96-25-13")

hist(tab_ctd_station$max_depth_ctd - tab_ctd_station$MAX_DEP)
hist(tab_ctd_station$distance_m)
hist(as.numeric(tab_ctd_station$dist_t))

thr_dist = 5000
thr_time = 6
tab_ctd_filt <- tab_ctd_station %>%
  group_by(REF, NUM) %>%
  filter(distance_m < thr_dist) %>% #___m
  arrange(dist_t,) %>%
  #filter(row_number() == 1) %>%
  filter(dist_t <= thr_time * 60) %>% #___min
  filter(max_depth_ctd >= MAX_DEP)

## End script
rm(list=ls())
