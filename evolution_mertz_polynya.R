## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-05
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
## ---------------------------
## Library
library(ggplot2)
library(dplyr)
library(terra)
library(rnaturalearth)
library(tidyterra)
library(raster)
library(ggthemes)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Destination projection
dest_proj <- "+proj=stere +lon_0=75 +lat_0=-90 +lat_ts=-70 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

## Continent
bbox <- extent(c(134, 160, -72, -65))
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox) 

wm_proj <- wm |>
  project(dest_proj)

## Polynya contours
all_cont <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

## Metadata polynya
info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")

id <- info %>%
  filter(Name == "Mertz") %>%
  pull(ID)

cont <- all_cont %>%
  filter(ID == id & month == "July" & year >= 2004 & year <= 2023) %>%
  mutate(event = case_when(year < 2010 ~ "pre velage",
                           year >= 2010 ~ "post velage"),
         new_id = paste0(year,".",ID,".", num))

cont$event <- factor(cont$event, levels <- c("pre velage", "post velage"))

# cont_proj <- cont |>
#   vect(geom =  c("longitude", "latitude"), crs = "EPSG:4326", "polygons") |>
#   project(dest_proj)


source("~/Desktop/WHOI/Codes/useful_functions/df_to_SpatialPolygonsDataFrame.R")

# pol <- df_to_SpatialPolygonsDataFrame(cont, "new_id")
# projection(pol) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# 
# cont_proj <- spTransform(pol, CRS(dest_proj))

ggplot() +
  # geom_spatvector(data = cont_proj) + # Tes points
  geom_polygon(data = cont_proj, aes(x = long, y = lat, group = id, col = id), fill = NA) +
  # geom_polygon(data = cont_proj, aes(x = lon, y = latitude, group = interaction(year, num), col = factor(event)), fill = NA) +
  geom_spatvector(data = wm) +
  xlab("Lon (°E)") +
  ylab("Lat (°N)") +
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA)
  )

ggplot() +
  geom_polygon(data = cont, aes(x = lon, y = lat, group = new_id, col = factor(year)),  fill = NA, alpha = .1) +
  geom_spatvector(data = wm) +
  xlim(c(134, 152)) +
  ylim(c(-68, -65)) +
  xlab("Lon (°E)") +
  ylab("Lat (°N)") +
  theme_map() +
  ggtitle("July") +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        strip.text.y = element_text(size = 20),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA)
  ) +
  facet_grid(event~.)

ggsave("~/Dropbox/data/outputs_Marthe_2023/evolution_mertz_velage.png", height = 40, width = 30, units = "cm", dpi = 300)

## End script
rm(list=ls())
