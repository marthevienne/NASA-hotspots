## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-20
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
library(ggthemes) # theme_map()
library(raster)
library(terra)
library(rnaturalearth)
library(tidyterra)
## ---------------------------

## Continents
bbox <- extent(c(-6, 160, -74, -60))
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)

## Import residence time raster
rt <- raster::brick("output_data/residence_time/tif/RES_0.2_residence_time_season.tif") #____read raster stack
res(rt)
res_filename = 0.2 # or "CESM_LR" #___variable in filename

## Crop raster?
crop <- FALSE

if (crop) {
  bbox <- extent(c(80, 100, -70, - 60))
  rt <- crop(rt, bbox)
}

names(rt)
## Type of residence time
type <- "SUM" # "SUM" | "MEAN" | "REL" | ""
rt_type <- subset(rt, grep(type, names(rt)))
max_val <- max(values(rt_type), na.rm = T)
max_val
#max_val <- log(max_val)
lim <- c(0, max_val)
freq_break <- 2 # days in colorbar

## Map residence time
limits <- c(0, 0.5, 1)# / rt[[type]]@data@max #___define limits
breaks <- seq(0, ceiling(max_val), freq_break)
rt_SR <- rast(rt_type)

for (i in 1:nlayers(rt_type)) {
  ## Figure filename
  file_name <- sprintf("~/Dropbox/data/outputs_Marthe_2023/residence_time/RES_%s_residence_time_sum_pixel_season_%s.png", res_filename, names(rt_SR)[i])
  
  map_RT <- ggplot() +  
    geom_spatraster(data = rt_SR[[i]])+
    geom_spatvector(data = wm) +
    scale_fill_gradientn("Time spent \nin pixels (in days)", 
                         colours = c("#FFFCE6","#FF0000FF", "#970000"), values = limits,
                         na.value = NA,
                         breaks = breaks, limits = lim) +
    xlab("") +
    ylab("") +
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
  
  ggsave(plot = map_RT, file_name, height = 20, width = 40, units = "cm", dpi = 300)
}

## End script
rm(list=ls())
