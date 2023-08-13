##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-12
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
setwd("~/Desktop/WHOI/Data/behavioural_data/")
## ---------------------------
## Library
library(htmlwidgets)
library(dplyr)
library(plotly)
library(raster)
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/functions_raster/raster_to_df.R")
## ---------------------------

## Dive data
dives <- readRDS("dive_metrics_bottime_speed_interp_hunttime_bathy_zone_5")
colnames(dives)

dives <- dives %>%
  mutate(dist_from_seafloor = abs(bathy) - MAX_DEP) #___dist > 0 => above bathymetry
range(dives$dist_from_seafloor, na.rm = T)

hist(dives$dist_from_seafloor, breaks = 50)
abline(v = 0)

dives %>%
  mutate(above_seafloor = case_when(dist_from_seafloor > 0 ~ TRUE,
                                    dist_from_seafloor <= 0 ~ FALSE)) %>%
  group_by(above_seafloor) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) #___dives deeper than bathy => 17%


dives_below_bathy <- dives %>% filter(dist_from_seafloor <= 0)

## Plot dives and bathymetry in 3D
r <- raster("~/Desktop/WHOI/Data/bathy_data/RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd")
bbox <- extent(c(-2, 151, -72, -60))
r <- crop(r, bbox)
r2 <- aggregate(r, fact = 100)
df_bathy <- raster_to_df(r2)

p2 <- plot_ly(width = 1000, height = 1000) %>% 
  add_trace(data = df_bathy,  x = ~x, y = ~y, z = ~value, type = "mesh3d", intensity = ~value) %>%
  add_markers(data = dives_below_bathy, x = ~interpLon, y = ~interpLat, z = ~-MAX_DEP, marker = list(size = 1, color = "black")) %>%
  layout(autosize = F, scene = list(aspectmode = "manual",
                                    aspectratio = list(x = 1, y = 0.5, z = 1)))


htmlwidgets::saveWidget(
  widget = p2,
  file = "~/Dropbox/data/outputs_Marthe_2023/dives_bathy/dives_below_bathy_3D.html",
  selfcontained = TRUE #___creates a single html file
)


p3 <- plot_ly(width = 1000, height = 1000) %>% 
  add_trace(data = df_bathy,  x = ~x, y = ~y, z = ~value, type = "mesh3d", intensity = ~value) %>%
  add_markers(data = dives, x = ~interpLon, y = ~interpLat, z = ~-MAX_DEP, marker = list(size = 1, color = "black")) %>%
  layout(autosize = F, scene = list(aspectmode = "manual",
                                    aspectratio = list(x = 1, y = 1, z = 1)))

htmlwidgets::saveWidget(
  widget = p3,
  file = "~/Dropbox/data/outputs_Marthe_2023/dives_bathy/dives_3D.html",
  selfcontained = TRUE #___creates a single html file
)


## Dives deeper set to zero
dives$dist_from_seafloor[which(dives$dist_from_seafloor <= 0)] <- 0

## Dive mode
dives <- dives %>%
  mutate(dive_mode = case_when(dist_from_seafloor > 70 ~ "pelagic",
                               dist_from_seafloor <= 70 ~ "benthic"))
dives %>%
  group_by(dive_mode) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) #___68% P / 32% benthic / 0.5% NA

## Save dives table
saveRDS(dives, "dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_6")

## Histograms max depth and distance to seafloor
{
pdf(
  "~/Dropbox/data/outputs_Marthe_2023/hist_max_depth.pdf",
  height = 5,
  width = 6
)
print(
  hist(
    dives$MAX_DEP,
    breaks = 50,
    xlab = "Maximum dive depth (m)",
    main = "Maximum dive depth histogram"
  )
)
dev.off()
}

{
  pdf(
    "~/Dropbox/data/outputs_Marthe_2023/hist_dist_from_seafloor.pdf",
    height = 5,
    width = 6
  )
  print(
    hist(
      dives$dist_from_seafloor,
      breaks = 50,
      xlab = "Distance from seafloor (m)",
      main = "Distance from seafloor histogram"
    )
  )
  print(abline(v = 70, lty = 2))
  dev.off()
}

## End script
rm(list=ls())
