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
## Paths
path_map <- "~/Dropbox/data/outputs_Marthe_2023/n_seals_pixels/"
## ---------------------------
## Library
library(ggthemes) # theme_map()
library(raster)
library(terra)
library(rnaturalearth)
library(tidyterra)
## ---------------------------

## Import raster
file <- "output_data/nseals/tif/RES_CESM_LR_nseals_sum_pixel_months.tif"
r <- raster::brick(file) #____read raster stack
base.name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))

## Crop raster?
crop <- FALSE

if (crop) {
  bbox <- extent(c(80, 100, -70, - 60))
  r <- crop(r, bbox)
}

names(r)
max_val <- max(values(r), na.rm = T)
max_val
lim <- c(1, max_val)
lim
freq_break <- 2 # days in colorbar

## Map parameters
limits <- c(0, 0.2, 1)# / rt[[type]]@data@max #___define limits
breaks <- seq(1, ceiling(max_val), freq_break)
r_SR <- rast(r)

## Continents
bbox <- extent(c(-6, 160, -74, -60))
wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox)

for (i in 1:nlayers(r)) {
  file_name <- sprintf("%s%s_%s.png", path_map, base.name, names(r_SR)[i]) #___figure filename

  map <- ggplot() +  
    geom_spatraster(data = r_SR[[i]])+
    geom_spatvector(data = wm) +
    scale_fill_gradientn("Total number \nof seals in pixels", 
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
  
  ggsave(plot = map, file_name, height = 20, width = 40, units = "cm", dpi = 300)
}

## End script
rm(list=ls())
