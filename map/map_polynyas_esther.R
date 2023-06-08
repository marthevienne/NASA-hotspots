##
## Script name: polynya_contours
##
## Purpose of script: 1) Open and rearrange polynya contours data from NCAR (netcdf) for SSMI and CESM data
##                        => create list object with lon, lat, z (NA or 1)
##                    2) Plot polynyas map
##                    3) Identify polynya
##
## Author: Marthe Vienne
##
## Date Created: 2023-04-25
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
rm(list = ls())
setwd("~/Desktop/WHOI/Data/")

## Library
library(ncdf4) # for netcdf manipulation
library(lubridate)
library(raster)
library(dplyr)
library(ggforce)

## Palette
source("~/Desktop/WHOI/Codes/Marthe_to_be_validated/palette_polynya.R")

## Pol
polynyas = read.csv("polynyas_contours/Esther/Ester_polynyas_contours_bigger_df.csv")
name_polynyas = read.csv("polynyas_contours/Esther/ID_polynyas_Esther.csv", sep = ";")
# ggplot() +
#   geom_polygon(data = polynyas, aes(x = lon, y = lat)) +
#   facet_wrap(~month)

## Dives
dives = readRDS("~/Desktop/WHOI/Data/output_data/interpoled_locations_dives_north_boundary_5meters")
dives$month_num = sapply(dives$DE_DATE, function(t) ifelse(month(t) < 10, 
                                                     paste0("0",  month(t)), 
                                                     paste0(month(t))))
dives$month = sapply(dives$DE_DATE, function(t) month.name[month(t)])
head(dives$month)


## Continent 
bbox <- ext(-5, 170, -80, -58.5)
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)# |> project(dest_proj)

polynyas$month = factor(polynyas$month, levels = month.name)
dives$month = factor(dives$month, levels = month.name)

map_pol_dives <- 
  ggplot() +
  geom_spatvector(data = wm)+
  geom_polygon(data = polynyas, aes(x = lon, y = lat, group = ID, col = as.factor(ID)), fill = NA) +
  scale_color_manual(values = pal, name = "Polynya", labels = name_polynyas$Name)+
  #scale_color_viridis_d()+
  geom_point(data = dives, mapping = aes(x = lon, y = lat), col = "darkgrey", size = .001, alpha = .2) +
  facet_wrap_paginate(~month, nrow = 12, ncol = 1) +
  lims(x = c(-15, 170), y = c(-73, -58.5))+
  theme_light()+
  xlab("Lon (°E)")+
  ylab("Lat (°N")

png("~/Dropbox/data/outputs_Marthe_2023/polynya contours/Esther/monthly_polynyas.png", height = 95, width = 55, units = "cm", res = 300)
print(map_pol_dives)
dev.off()
