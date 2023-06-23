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
# dest_proj <- "+proj=stere +lon_0=75 +lat_0=-90 +lat_ts=-70 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

## NA grid raster
grid <- raster(xmn = -6, xmx = 160, ymn = -72, ymx = -60)
res(grid) <- 0.2 #°
projection(grid) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

## Continents
bbox <- extent(c(-6, 155, -90, -60))
wm <- wm <- rnaturalearth::ne_download(returnclass = "sf", scale = "large") |>
  vect() |>
  crop(bbox) 

# wm_proj <- wm |>
#   project(dest_proj)

# wm_df <- crds(wm, df = T)
# wm_df <- wm_df %>%
#   filter(y > 10000) %>%
#   arrange_at("x")
# 
# wm_l <- Line(wm_df)
# wm_ll <- Lines(list(wm_l), ID = "Antarctica")
# SP_wm <- SpatialLines(list(wm_ll))

#==================================================================
# 1) RESIDENCE TIME ON ALL PERIOD
#==================================================================

d <- dives
coordinates(d) <- c("lon", "lat")
projection(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

## Total residence time (all years)
sum_days <- function(x, ...) { sum(na.omit(x)) / 3600 / 24}
r = rasterize(d, grid, "dive_duration", fun = sum_days)
r_proj <- projectRaster(r, crs = dest_proj) #___reproject dives data

# grad <- colorRampPalette(c("#FCFFDD", "red"))

# tiff(sprintf("~/Dropbox/data/outputs_Marthe_2023/residence_time/RES_%s_RT_total_2004-2023.tiff", res(grid)[1]), height = 20, width = 35, units = "cm", res = 300)
file_name <- sprintf("~/Dropbox/data/outputs_Marthe_2023/residence_time/RES_%s_RT_total_2004-2023.pdf", res(grid)[1])
# plot(t(flip(r_proj, 2)))
pdf(file_name, height = 4, width = 11)
plot(
  r, useRaster = F, 
  legend.args = list(
    text = "Time spent \n(days)"
  )
)
box(col = "white") # Add box to plot
plot(wm, add = TRUE)
dev.off()

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
          time_spent) %>%
  mutate(rel_time_spent = time_spent / n_seals)

#==================================================================
# 3) RESIDENCE TIME BY CELL (mean)
#==================================================================

tsp <- tsp_tot %>%
  group_by(cell) %>%
  reframe(mean_time_spent = mean(time_spent),
          tot_time_spent = sum(time_spent),
          tot_rel_time_spent = sum(rel_time_spent))

xyCell = xyFromCell(grid, tsp$cell)
tsp <- tsp %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(tsp) <- c("lon", "lat")

## Construct raster layer brick for each month
r_mean <- rasterize(tsp, grid, "mean_time_spent")
r_sum <- rasterize(tsp, grid, "tot_time_spent")
r_rel <- rasterize(tsp, grid, "tot_rel_time_spent")

r <- do.call(brick, c(r_sum, r_mean, r_rel))
names(r) <- c("SUM", "MEAN", "REL")

file_output = sprintf("~/Desktop/WHOI/Data/output_data/RES_%s_residence_time_period.tif", res(r)[1])
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)


#==================================================================
# 4) RESIDENCE TIME BY MONTH
#==================================================================

## => total, mean and relative time spent in each grid cell for a given month
tsp <- tsp_tot %>%
  group_by(month, cell) %>%
  reframe(mean_time_spent = mean(time_spent),
          tot_time_spent = sum(time_spent),
          tot_rel_time_spent = sum(rel_time_spent))

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
  lyr_rel <- rasterize(tsp_month, grid, "tot_rel_time_spent")
  
  r_mean[[m]] <- lyr_mean
  r_sum[[m]] <- lyr_sum
  r_rel[[m]] <- lyr_rel
}

r <- do.call(brick, c(r_sum, r_mean, r_rel))
names(r) <- paste0(rep(c("SUM.", "MEAN.", "REL."), each = length(months)), rep(month.name, 3))

file_output = sprintf("~/Desktop/WHOI/Data/output_data/RES_%s_residence_time_month.tif", res(r)[1])
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

#==================================================================
# 4) RESIDENCE TIME BY SEASON 
#==================================================================

## => total time spent in a grid cell by season
tsp_season <- tsp_tot %>%
  mutate(season = case_when(month == 2 ~ 1,
                            month >= 3 & month <= 5 ~ 2,
                            month >= 6 & month <= 8 ~ 3,#___ 1 = "Summer", 2 = "Autumn", 3 = "Winter", 4 = "Spring" 
                            .default = 4)) %>%
  group_by(cell, season) #%>%
  reframe(mean_time_spent = mean(time_spent),
          tot_time_spent = sum(time_spent),
          tot_rel_time_spent = sum(rel_time_spent))

xyCell = xyFromCell(grid, tsp_season$cell)
tsp_season <- tsp_season %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(tsp_season) <- c("lon", "lat")

## Construct raster layer brick for each month
seasons <- unique(tsp_season$season)
r_mean <- list()
r_sum <- list()
r_rel <- list()

for (m in seasons) {
  tsp_month <- tsp_season[tsp_season$season == m, ]
  
  lyr_mean <- rasterize(tsp_month, grid, "mean_time_spent")
  lyr_sum <- rasterize(tsp_month, grid, "tot_time_spent")
  lyr_rel <- rasterize(tsp_month, grid, "tot_rel_time_spent")
  
  r_mean[[m]] <- lyr_mean
  r_sum[[m]] <- lyr_sum
  r_rel[[m]] <- lyr_rel
}

r <- do.call(brick, c(r_sum, r_mean, r_rel))
names(r) <- paste0(rep(c("SUM", "MEAN", "REL"), each = length(seasons)), rep(c(".Summer", ".Autumn", ".Winter", ".Spring"), 3))

file_output = sprintf("~/Desktop/WHOI/Data/output_data/RES_%s_residence_time_season.tif", res(r)[1])
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

## End script
rm(list=ls())
