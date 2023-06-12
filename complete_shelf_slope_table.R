##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-09
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
setwd("~/Desktop/WHOI/Data/bathy_data/")
## ---------------------------
## Library
library(raster)
library(plotly)
## ---------------------------

source("~/Desktop/WHOI/Codes/useful_functions/zonal_transect_bathy.R")
source("~/Desktop/WHOI/Codes/useful_functions/find_lat_end_slope.R")
source("~/Desktop/WHOI/Codes/useful_functions/add_row_to_df.R")

## Bathymetry data
bathy <- raster("RES_0.0041_BATHY_gebco_2023_sub_ice_n-60.0_s-74.0_w-6.0_e170.0.grd")
bathy
ext <- extent(bathy)

## Bathymetric limits
table <- read.csv("shelf_slope_Marthe.txt", sep = ",")
range(table$lon)

## Longitude to explore
lon <- -6
row <- as.data.frame(matrix(NA, ncol = 3))
colnames(row) <- colnames(table)
row$lon <- lon

transect <- zonal_transect_bathy(bathy, lon)

p_shelf <- plot_ly(data = transect,  x = ~lat, y = ~bathymetry,
                   marker = list(size = 1, color = "black")) %>% 
  add_paths()

row$end_slope <- find_lat_end_slope(transect)

if (is.na(row$end_slope)) {
  print(p_shelf)
}
# row$end_slope <- # ?

## Find end shelf
p_shelf
row$end_shelf <- -70.29 # ?
row

new_table <- add_row_to_df(table, row, order_by = "lon")

## Save updated table
write.csv(new_table, "shelf_slope_Marthe.txt", row.names = F)

## End script
rm(list=ls())
