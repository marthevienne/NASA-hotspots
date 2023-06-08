rm(list=ls())

setwd("~/Desktop/WHOI/Data/")

## Library
library(tidyterra)
library(terra)
library(ggplot2)
library(ggforce)

## Low res polynyas
hi0.4 = read.csv("polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021_df.csv")
colnames(hi0.4) = c("lon", "lat", "pol", "month")
hi0.4 <- hi0.4[hi0.4$pol != 0, ]
hi0.4$month = factor(hi0.4$month, levels = month.name)

aice85 = read.csv("polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021_df.csv")
colnames(aice85) = c("lon", "lat", "pol", "month")
aice85 <- aice85[aice85$pol != 0, ]
aice85$month = factor(aice85$month, levels = month.name)

ggplot() +
  geom_tile(data = aice85, aes(x = lon, y = lat, fill = pol))

## Observations polynyas
obs_pol = read.csv("polynyas_contours/OBS/all_contours_per_month/OBS_union_polynyas_contours_df.csv")
obs_pol$month <- factor(obs_pol$month, levels = month.name)
obs_pol$ID <- as.factor(obs_pol$ID)

# obs_pol_big = read.csv("polynyas_contours/OBS/biggest_contours_per_month/OBS_biggest_polynyas_contours_df.csv")
obs_pol_big = read.csv("polynyas_contours/OBS/all_contours_per_month/OBS_big_polynyas_contours_df.csv")
obs_pol_big$month <- factor(obs_pol_big$month, levels = month.name)
obs_pol_big$ID <- as.factor(obs_pol_big$ID)

## Map per month
dives = readRDS("~/Desktop/WHOI/Data/output_data/filtered_numbered_dives") #___ TO REPLACE BY INTERP DIVES

dives$month_num = sapply(dives$DE_DATE, function(t) ifelse(month(t) < 10, 
                                                           paste0("0",  month(t)), 
                                                           paste0(month(t))))
dives$month = sapply(dives$DE_DATE, function(t) month.name[month(t)])
head(dives$month)
dives$month = factor(dives$month, levels = month.name)

## Continent 
bbox <- ext(-5, 170, -80, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)

map_pol_dives <- 
  ggplot() +
  geom_spatvector(data = wm)+
  # geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "darkgrey", size = .001, alpha = .2) +
  #geom_tile(data = hi0.4, aes(x = lon, y = lat), fill = "blue", alpha = .6) +
  geom_tile(data = aice85, aes(x = lon, y = lat), fill = "maroon", alpha = .6) +
  geom_polygon(data = obs_pol, aes(x = lon, y = lat, group = interaction(ID, num)), col = "red", fill = NA, alpha = .3, linewidth = .4) +
  geom_point(data = dives, mapping = aes(x = LON, y = LAT), col = "darkgrey", size = .001, alpha = .2) +
  #scale_color_viridis_d()+
  facet_wrap_paginate(~month, nrow = 12, ncol = 1) +
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N")+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.x = element_text(size = 30))

#file_output = "~/Dropbox/data/outputs_Marthe_2023/polynya contours/compare_contours/hi0.4_unionObs.png"
file_output = "~/Dropbox/data/outputs_Marthe_2023/polynya contours/compare_contours/aice85_unionObs.png"
ggsave(plot = map_pol_dives, filename = file_output, width = 50, height = 100, units = "cm", dpi = 150)


map_pol_dives_big <- 
  ggplot() +
  geom_spatvector(data = wm)+
  # geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "darkgrey", size = .001, alpha = .2) +
  # geom_tile(data = hi0.4, aes(x = lon, y = lat), fill = "blue", alpha = .6) +
  geom_tile(data = aice85, aes(x = lon, y = lat), fill = "maroon", alpha = .6) +
  geom_polygon(data = obs_pol_big, aes(x = lon, y = lat, group = ID), col = "red", fill = NA, alpha = .3, linewidth = .4) +
  geom_point(data = dives, mapping = aes(x = LON, y = LAT), col = "darkgrey", size = .001, alpha = .2) +
  #scale_color_viridis_d()+
  facet_wrap_paginate(~month, nrow = 12, ncol = 1) +
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N")+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.x = element_text(size = 30))

#file_output = "~/Dropbox/data/outputs_Marthe_2023/polynya contours/compare_contours/hi0.4_bigObs.png"
file_output = "~/Dropbox/data/outputs_Marthe_2023/polynya contours/compare_contours/aice85_bigObs.png"
ggsave(plot = map_pol_dives, filename = file_output, width = 50, height = 100, units = "cm", dpi = 150)


map_pol_dives_obs <- 
  ggplot() +
  geom_spatvector(data = wm)+
  # geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "darkgrey", size = .001, alpha = .2) +
  geom_polygon(data = obs_pol, aes(x = lon, y = lat, group = interaction(ID, num), fill = ID), alpha = .6) +
  geom_polygon(data = obs_pol_big, aes(x = lon, y = lat, group = interaction(ID, num), col = ID), fill = NA, alpha = .6, linewidth = .6) +
  #scale_color_viridis_d()+
  facet_wrap_paginate(~month, nrow = 12, ncol = 1) +
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N")+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.x = element_text(size = 30))

file_output = "~/Dropbox/data/outputs_Marthe_2023/polynya contours/compare_contours/bigObs_unionObs.png"
ggsave(plot = map_pol_dives_obs, filename = file_output, width = 50, height = 100, units = "cm", dpi = 200)

