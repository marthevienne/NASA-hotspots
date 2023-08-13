setwd("~/Desktop/WHOI/Data/")

## Select polynya
library(plotly)
library(lubridate)
library(raster)

source("~/Desktop/NASA-hotspots/functions_raster/raster_to_df.R")
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")
selection

## Polynyas info
polynya_info <- read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
names_pol <- polynya_info$Name
names(names_pol) = polynya_info$ID

## Bathymetry
r <- raster("~/Desktop/WHOI/Data/bathy_data/RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd")
r_above_ice <- raster("~/Desktop/WHOI/Data/bathy_data/RES_0.0041_TOPO_gebco_2023_n-58.0_s-74.0_w-6.0_e170.0.grd")

for (id_pol in selection) {
  
  ## Polynya (union)
  polynyas <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_union_all_months_polynyas_contours_df.csv")
  polynyas <- polynyas %>%
    filter(ID == id_pol)
  
  polynyas$z = 0
  
  ## Dives in polynyas
  dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
    filter(!is.na(dive_mode)) %>%
    mutate(month = month(DE_DATE),
           year = year(DE_DATE)) %>%
    filter(month <= 8) #___summer-winter
  
  dives <- dives %>%
    filter(pol == id_pol)
  
  range(dives$interpLat, na.rm = T)
  
  max_depth <- max(dives$MAX_DEP)
  
  ## Plot dives and bathymetry in 3D
  bbox <- extent(c(range(polynyas$lon),  range(polynyas$lat)))
  r2 <- crop(r, bbox)
  r3 <- aggregate(r2, fact = 10)
  df_bathy <- raster_to_df(r3)
  
  r2_above <- crop(r_above_ice, bbox)
  r3_above <- aggregate(r2_above, fact = 10)
  #r3_above[r3_above > 0] <- 0
  df_bathy_above <- raster_to_df(r3_above)
  max_topo <- max(df_bathy_above$value)
  
  p2 <- plot_ly(width = 1000, height = 1000) %>% 
    add_markers(data = dives, x = ~interpLon, y = ~interpLat, z = ~-MAX_DEP, color = ~factor(year), symbol = ~REF,
                marker = list(size = 2)) %>%
    add_trace(data = df_bathy,  x = ~x, y = ~y, z = ~value,
              type = "mesh3d", 
              #intensity = ~value, colorscale = 'viridis'
              facecolor = rep("lightgrey", nrow(df_bathy)*2)
              ) %>%
    add_trace(data = df_bathy_above,  x = ~x, y = ~y, z = ~value,
              type = "mesh3d", 
              #intensity = ~value, colorscale = 'viridis'
              facecolor = rep("white", nrow(df_bathy)*2)
              ) %>%
    add_paths(x = polynyas$lon, y = polynyas$lat, z = polynyas$z,
              split = polynyas$num,
              line = list(color = "black")) %>%
    layout(autosize = F, scene = list(aspectmode = "manual",
                                      aspectratio = list(x = 1, y = .8, z = 1), zaxis = list(range = c(-(max_depth + 50), max_topo)))) %>% 
    layout(legend = list(orientation = "h",  xanchor = "center", x = 0.5, y = -0.1))
  
  pol_name <- names_pol[[id_pol]]
  htmlwidgets::saveWidget(
    widget = p2,
    file = sprintf("~/Dropbox/data/outputs_Marthe_2023/dives_bathy/per polynya/%s.html", pol_name),
    selfcontained = TRUE #___creates a single html file
  )
}


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