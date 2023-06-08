rm(list=ls())

library(ggplot2)
library(base)
library(ggfortify)
library(dplyr)
library(rnaturalearth)

setwd("~/Desktop/WHOI/Data/")

## Bathymetry and coast
topo <- raster("~/Dropbox/data/bathymetry/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")
extopo <- crop(topo, extent(-2, 160, -75,-58.5))
extopo[extopo > 0] <-NA
writeRaster(extopo, "bathy_zone_Marthe.grd")

extopo <- raster("bathy_zone_Marthe.grd")

## Dives
dives = readRDS("interpoled_locations_dives_north_boundary_5meters")
 
# ## CESM polynyas contours
# poly_cesm = readRDS("~/Dropbox/data/polynya_contours_NCAR_2023/polynyas_polygons_CESM")
# poly_cesm = as.data.frame(poly_cesm)
# poly_cesm_in_region = subset(poly_cesm, subset = lon > -2 & lon <= 160 & lat > -74 & lat < -58)
# 
# ## SSMI polynyas contours
# poly_ssmi = readRDS("~/Dropbox/data/polynya_contours_NCAR_2023/polynyas_polygons_SSMI")
# poly_ssmi = as.data.frame(poly_ssmi)
# poly_ssmi_in_region = subset(poly_ssmi, subset = lon > -2 & lon <= 160 & lat > -74 & lat < -58)


antarctica <- ne_countries(scale = "large", returnclass = "sf", continent = "Antarctica")

antarctica_map <- antarctica %>% 
  dplyr::group_by(continent) %>% 
  dplyr::select(continent)


ggplot()+
  geom_sf(data = antarctica_map, 
          aes(geometry = geometry), 
          inherit.aes = F, 
          fill = "grey")+
  coord_sf(xlim = c(65, 105), ylim = c(-69, -64), expand = FALSE)



## Polynyas CESM
r = readRDS("~/Dropbox/data/polynya_contours_NCAR_2023/polynyas_raster_CESM")
#

## Polynyas SSMI
r = readRDS("~/Dropbox/data/polynya_contours_NCAR_2023/polynyas_polygons_SSMI")
SSMI_df <- as.data.frame(r, xy = TRUE) %>% 
  na.omit()

colnames(SSMI_df) = c("lon", "lat", "poly_id")
head(SSMI_df)

SSMI_df_sub = subset(SSMI_df, subset = lon > -2 & lon <= 160 & lat > -74 & lat < -58)

winter_dives = subset(dives, subset = month(DE_DATE) < 11 & month(DE_DATE) >= 7)

## Map with bathy, dives, polynyas contours
p1 <- autoplot(extopo, geom=c("raster","contour"), coast = TRUE, 
              mapping = aes(levels(bins=30)))
  
  p1 <- ggplot()+
  geom_sf(data = antarctica_map, 
          aes(geometry = geometry), 
          inherit.aes = F, 
          fill = "grey")+
  coord_sf(xlim = c(-2, 170), ylim = c(-74, -58.5), expand = FALSE)+
  geom_point(data = winter_dives, aes(x=lon,y=lat),
             shape = 16, colour = "orange",fill="NA", size = .1, alpha = .4,
             show.legend = TRUE) +
  geom_tile(data = SSMI_df_sub, mapping = aes(x = lon, y = lat), 
            fill="blue", alpha = .5, show.legend = T)+
  # geom_tile(data = poly_ssmi_in_region, mapping = aes(x = lon, y = lat, group = id_poly), 
  #              color = "pink", fill = "pink", alpha = .4) +
  labs(x = "Lon (°E)",
       y = "Lat (°N)",
       fill = "Bathymetry (m)")+
    theme_bw()
  
  ggsave("map_winter_dives_SSMI_SES.png", height = 10, width = 40, units = c("cm"), dpi = 300)
  
  
  p2 <- ggplot()+
    geom_sf(data = antarctica_map, 
            aes(geometry = geometry), 
            inherit.aes = F, 
            fill = "grey")+
    coord_sf(xlim = c(-2, 170), ylim = c(-74, -58.5), expand = FALSE)+
    geom_point(data = winter_dives, aes(x=lon,y=lat),
               shape = 16, colour = "orange",fill="NA", size = .1, alpha = .4,
               show.legend = TRUE) +
    geom_tile(data = CESM_df_sub, mapping = aes(x = lon, y = lat), 
              fill="black", alpha = .5, show.legend = T) +
    # geom_tile(data = poly_ssmi_in_region, mapping = aes(x = lon, y = lat, group = id_poly), 
    #              color = "pink", fill = "pink", alpha = .4) +
    labs(x = "Lon (°E)",
         y = "Lat (°N)",
         fill = "Bathymetry (m)")+
    theme_bw()
  
  ggsave("map_winter_dives_CESM_SES.png", height = 10, width = 40, units = c("cm"), dpi = 300)
  

ggsave("map_dive_poly_SES.png", height = 10, width = 40, units = c("cm"), dpi = 300)

ggsave("test_map.png", height = 10, width = 40, units = c("cm"), dpi = 300)


library(marmap)
# bathy <- as.bathy(topo)
# bf = fortify.bathy(bathy)
# saveRDS(bf, "bathy_df")

p2 <- ggplot() +
  geom_contour(bf, mapping = aes(x = x, y = y, z = z),
               breaks=c(-100),
               colour="grey")+
  geom_sf(data=world_map, col=2, alpha=.8)+
  coord_sf(xlim = c(-2, 170), ylim = c(-74, -58.5), expand = FALSE)+
  borders("world",alpha=.3, fill="grey",colour="black")

ggsave("test_map.png", height = 10, width = 40, units = c("cm"), dpi = 150)

  
## CALCULATE POLYNYAS AREA
library(tibble)
poly_cesm_in_region = as_tibble(poly_cesm_in_region)

poly <- poly_cesm_in_region %>% group_by(id_poly)
poly$lon_360 = sapply(poly$lon,
                      function(lon)
                        ifelse(lon < 0, lon + 360, lon))
library(dplyr)
split_poly = group_split(poly)

poly_areas = lapply(split_poly, function(poly) polyarea(poly$lon_360, abs(poly$lat)))
