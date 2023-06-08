rm(list=ls())

library(marmap)
library(ggnewscale)
library(lubridate)
library(raster)
library(ggplot2)

path_fig = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")


source("~/Desktop/WHOI/Codes/palettes/palette_bathy.R")
source("~/Desktop/WHOI/Codes/Marthe_to_be_validated/palette_polynya.R")

setwd("~/Desktop/WHOI/Data/")

## Bathy data
bathy_EA <- readRDS("bathy_data/LR_BATHY_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0")

## Polynya data
CESM_df <- readRDS("polynyas_contours/CESM_df_pol_id")
SSMI_df <- readRDS("polynyas_contours/SSMI_df_pol_id")
CESM_df_w <- readRDS("polynyas_contours/WINTER_CESM_df_pol")
SSMI_df_w <- readRDS("polynyas_contours/WINTER_SSMI_df_pol")

## Dives
dives = readRDS("output_data/interpoled_locations_dives_north_boundary_5meters")

## Palette
palette_bathy <- palette.bathy(mat = bathy_EA,
                               layers = list(c(min(bathy_EA, na.rm = T), 0, col_d2, col_d1, col_d0)),
                               land = FALSE, default.col = "grey" )


topo_EA <- raster("bathy_data/LR_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.grd")
df <- as.data.frame(topo_EA, xy = TRUE)

dives$season = NA
# dives$season[which(month(dives$DE_DATE) >= 1 & month(dives$DE_DATE) < 4)] = "January-March"
# dives$season[which(month(dives$DE_DATE) >= 7 & month(dives$DE_DATE) < 10)] = "July-September"
# dives$season[which(month(dives$DE_DATE) >= 4 & month(dives$DE_DATE) < 7)] = "April-June"
# dives$season[which(month(dives$DE_DATE) >= 10 & month(dives$DE_DATE) <= 12)] = "October-December"
# dives$season_f = factor(dives$season, levels=c("January-March","April-June","July-September","October-December"))

dives$season[which(month(dives$DE_DATE) >= 4 & month(dives$DE_DATE) < 11)] = "Winter"
dives$season[is.na(dives$season)] = "Summer"

  p1 <- ggplot()+
    geom_tile(data = df, 
            aes(x = x, y = y, fill = layer), 
            inherit.aes = F) +
    coord_quickmap()+
    scale_fill_continuous("Bathymetry (m)", na.value = "lightgrey")+
    new_scale_fill() +
    geom_tile(data = CESM_df, mapping = aes(x = lon, y = lat, fill = as.factor(poly_id)), 
               show.legend = T, inherit.aes = F)+
    lims(x = c(-5, 170), y = c(-73, -58.5))+
    scale_fill_manual("Polynya id", values = pal_CESM_met, guide = )+
    labs(x = "Lon (°E)",
         y = "Lat (°N)")+
    geom_point(data = dives, aes(x = lon,y = lat),
               shape = 16, colour = "black",fill="NA", size = .1, alpha = .2,
               show.legend = TRUE)+
    theme(axis.text=element_text(size = 17),
          axis.title=element_text(size = 20,face="bold"),
          legend.text=element_text(size = 17), #__________ legend texts
          legend.title=element_text(size = 17,face="bold"))

  

ggsave(paste0(path_fig, "map_polynya_id_CESM.png"), height = 20, width = 60, units = c("cm"), dpi = 150)
  
p2 <- ggplot()+
  geom_tile(data = df, 
            aes(x = x, y = y, fill = layer), 
            inherit.aes = F) +
  coord_quickmap()+
  scale_fill_continuous("Bathymetry (m)", na.value = "lightgrey")+
  geom_point(data = dives, aes(x = lon,y = lat), col = "black",
             shape = 16, size = .1, alpha = .2)+
  geom_tile(data = CESM_df_w, mapping = aes(x = lon, y = lat), fill = "#ED6CBF", inherit.aes = F, alpha = .6, show.legend = F)+
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  labs(x = "Lon (°E)",
       y = "Lat (°N)", title = "CESM - winter polynyas")+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.y = element_text(size = 30))+
  facet_grid(season~.)


ggsave(paste0(path_fig, "WS_map_WINTER_polynya_CESM.png"), height = 30, width = 60, units = c("cm"), dpi = 150)

p3 <- ggplot()+
  geom_tile(data = df, 
            aes(x = x, y = y, fill = layer), 
            inherit.aes = F) +
  coord_quickmap()+
  geom_point(data = dives, aes(x = lon,y = lat),
             shape = 16, colour = "black",fill="NA", size = .1, alpha = .2,
             show.legend = TRUE)+
  scale_fill_continuous("Bathymetry (m)", na.value = "lightgrey")+
  geom_tile(data = SSMI_df_w, mapping = aes(x = lon, y = lat), fill = "yellow", 
            show.legend = T, inherit.aes = F, alpha = .6)+
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  labs(x = "Lon (°E)",
       y = "Lat (°N)", title = 'SSMI - winter polynyas')+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.y = element_text(size = 30))+
  facet_grid(season~.)

ggsave(paste0(path_fig, "WS_map_WINTER_polynya_SSMI.png"), height = 30, width = 60, units = c("cm"), dpi = 150)


p4 <- ggplot()+
  geom_tile(data = df, 
            aes(x = x, y = y, fill = layer), 
            inherit.aes = F) +
  coord_quickmap()+
  scale_fill_continuous("Bathymetry (m)", na.value = "lightgrey")+
  geom_point(data = dives, aes(x = lon,y = lat), col = "black",
             shape = 16, size = .1, alpha = .2)+
  geom_tile(data = SSMI_df, mapping = aes(x = lon, y = lat), fill = "yellow", 
            show.legend = T, inherit.aes = F, alpha = .6)+
  geom_tile(data = CESM_df, mapping = aes(x = lon, y = lat), fill = "#ED6CBF", inherit.aes = F, alpha = .6, show.legend = F)+
  lims(x = c(-5, 170), y = c(-73, -58.5))+
  labs(x = "Lon (°E)",
       y = "Lat (°N)", title = "CESM (pink) and SSMI (yellow)")+
  theme(axis.text=element_text(size = 17),
        axis.title=element_text(size = 20,face="bold"),
        legend.text=element_text(size = 17), #__________ legend texts
        legend.title=element_text(size = 17,face="bold"),
        title = element_text(size = 30,face="bold"),
        strip.text.y = element_text(size = 30))+
  facet_grid(season_f~.)


ggsave(paste0(path_fig, "map_polynya_CESM_SSMI.png"), height = 50, width = 60, units = c("cm"), dpi = 300)

