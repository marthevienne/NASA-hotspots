## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-18
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
library(dplyr)
library(ggforce)
library(tidyterra)
library(terra)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------
## Palette
source("~/Desktop/NASA-hotspots/palettes/palette_polynya.R")
## ---------------------------

## Polynyas
polynyas = read.csv("polynyas_contours/OBS/all_contours_per_month/OBS_union_polynyas_contours_df.csv")
name_polynyas = read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")

## All dives (with and without ctd stations)
dives_all = readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_8") %>%
  filter(pol > 0)

## Only dives with ctd stations
dives_ctd = readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0 & !is.na(id_ctd))

dives_ctd$month_num = sapply(dives_ctd$DE_DATE, function(t) ifelse(month(t) < 10, 
                                                           paste0("0",  month(t)), 
                                                           paste0(month(t))))
dives_ctd$month = sapply(dives_ctd$DE_DATE, function(t) month.name[month(t)])
head(dives_ctd$month)

dives_all$month_num = sapply(dives_all$DE_DATE, function(t) ifelse(month(t) < 10, 
                                                           paste0("0",  month(t)), 
                                                           paste0(month(t))))
dives_all$month = sapply(dives_all$DE_DATE, function(t) month.name[month(t)])

## Continent 
bbox <- ext(-5, 170, -80, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)

polynyas$month = factor(polynyas$month, levels = month.name)
dives_ctd$month = factor(dives_ctd$month, levels = month.name)
dives_all$month = factor(dives_all$month, levels = month.name)

map_pol_dives <- 
  ggplot() +
  geom_spatvector(data = wm)+
  geom_polygon(data = polynyas, aes(x = lon, y = lat, group = interaction(ID, num), col = as.factor(ID)), fill = NA) +
  scale_color_manual(values = pal, name = "Polynya", labels = name_polynyas$Name)+
  geom_point(data = dives_all, mapping = aes(x = interpLon, y = interpLat), col = "darkgrey", size = .001, alpha = 1) +
  geom_point(data = dives_ctd, mapping = aes(x = interpLon, y = interpLat), col = "black", size = .001, alpha = .6) +
  facet_wrap_paginate(~month, nrow = 6, ncol = 2) +
  xlim(x = c(-15, 170)) +
  ylim(y = c(-72, -63)) +
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N)")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white"))


png("~/Dropbox/data/outputs_Marthe_2023/map_dives_zone/map_dives_ctd_polynya.png", height = 45, width = 100, units = "cm", res = 300)
print(map_pol_dives)
dev.off()

#------------------------------------------------------------------
# PRYDZ BAY
#------------------------------------------------------------------

prydz_bay <- map_pol_dives +
  xlim(c(60, 90)) +
  ylim(c(-70, -65))

png("~/Dropbox/data/outputs_Marthe_2023/map_dives_zone/prydz_bay_dives_ctd.png", height = 60, width = 50, units = "cm", res = 300)
print(prydz_bay)
dev.off()

## End script
rm(list=ls())
