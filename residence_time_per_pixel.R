## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-15
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
library(dplyr)
library(raster)
## ---------------------------

## Import dives data
dives <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_V8")

dives <- dives %>%
  select(c(REF, lon = interpLon, lat = interpLat, time = DE_DATE, dive_duration = DIVE_DUR)) %>%
  mutate(month = month(time),
         year = year(time))

## Destination projection
dest_proj <- "+proj=longlat +datum=WGS84 +no_defs"

## NA grid raster
grid <- raster(xmn = -6, xmx = 160, ymn = -72, ymx = -60)
res(grid) <- 1/5 #°
projection(grid) <- dest_proj

#==================================================================
# 1) RESIDENCE TIME ON ALL PERIOD
#==================================================================
d <- dives
coordinates(d) <- c("lon", "lat")
projection(d) <- dest_proj

## Total residence time (all years)
sum_days <- function(x, ...) { sum(na.omit(x)) / 3600 / 24}
r = rasterize(d, grid, "dive_duration", fun = sum_days)
plot(r)

#==================================================================
# 2) RESIDENCE TIME BY MONTH AND BY YEAR
#==================================================================

lonLat <- dives %>%
  select(c(lon, lat)) #___get dives coordinates

cells = cellFromXY(grid, lonLat) #___get cells index from dives coordinates

## => total time spent in a grid cell for a given month in a given year
tsp_tot <- dives %>%
  mutate(cell = cells) %>%
  group_by(REF, month, year, cell) %>%
  reframe(time_spent = sum(dive_duration / 3600 / 24)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(month, year, cell) %>%
  reframe(n_seals = n(),
          time_spent) 

## => total, mean and relative time spent in each grid cell for a given month
tsp <- tsp_tot %>%
  group_by(month, cell) %>%
  reframe(mean_time_spent = mean(time_spent),
          tot_time_spent = sum(time_spent),
          rel_time_spent = sum(time_spent) / sum(n_seals))


xyCell = xyFromCell(grid, tsp$cell)
tsp <- tsp %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(tsp) <- c("lon", "lat")

## Construct raster layer brick for each month
months = seq(1, 12)
r_mean <- list()
r_sum <- list()
r_rel <- list()

for (m in months) {
  tsp_month <- tsp[tsp$month == m, ]
  
  lyr_mean <- rasterize(tsp_month, grid, "mean_time_spent")
  lyr_sum <- rasterize(tsp_month, grid, "tot_time_spent")
  lyr_rel <- rasterize(tsp_month, grid, "rel_time_spent")
  
  r_mean[[m]] <- lyr_mean
  r_sum[[m]] <- lyr_sum
  r_rel[[m]] <- lyr_rel
}

r_mean <- do.call(brick, r_mean)
names(r_mean) <- month.name #___layer names

r_sum <- do.call(brick, r_sum)
names(r_sum) <- month.name #___layer names

r_rel <- do.call(brick, r_rel)
names(r_rel) <- month.name #___layer names

#==================================================================
# 3) CROP ALL 
#==================================================================


## End script
rm(list=ls())
