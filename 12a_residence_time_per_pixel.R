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
library(lubridate)
library(terra)
## ---------------------------

## Import dives data
dives <- readRDS("~/Desktop/WHOI/Data/behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_8")

dives <- dives %>%
  dplyr::select(c(REF, lon = interpLon, lat = interpLat, time = DE_DATE, dive_duration = DIVE_DUR)) %>%
  mutate(month = month(time),
         year = year(time))

## Destination projection
dest_proj <- "+proj=stere +lon_0=75 +lat_0=-90 +lat_ts=-70 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

## NA grid raster based on CESM extent grid
filename = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021.tif"
r <- raster(filename)
grid <- raster(extent(r))
e <- extent(-6, 160, -72, -59) #___box containing all the dives
# res(grid) <- res(r) #° CESML_LR
res(grid) <-  0.2 #°
grid <- crop(grid, e)

res_filename = 0.2 #"CESM_LR"
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

plot(r)
# grad <- colorRampPalette(c("#FCFFDD", "red"))

# tiff(sprintf("~/Dropbox/data/outputs_Marthe_2023/residence_time/RES_%s_RT_total_2004-2023.tiff", res_filename), height = 20, width = 35, units = "cm", res = 300)
file_name <- sprintf("~/Dropbox/data/outputs_Marthe_2023/residence_time/RES_%s_total_time_spent_2004-2023.pdf", res_filename)

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
  dplyr::select(c(lon, lat)) #___get dives coordinates

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

file_output = sprintf("output_data/residence_time/tif/RES_%s_residence_time_period.tif", res_filename)
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

file_output = sprintf("output_data/residence_time/tif/RES_%s_residence_time_month.tif", res_filename)
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

#==================================================================
# 4) RESIDENCE TIME BY SEASON 
#==================================================================

## => total time spent in a grid cell by season
tsp_season <- tsp_tot %>%
  mutate(season = case_when(month <= 2 ~ 1,
                            month >= 3 & month <= 5 ~ 2,
                            month >= 6 & month <= 8 ~ 3,#___ 1 = "Summer", 2 = "Autumn", 3 = "Winter", 4 = "Spring" 
                            .default = 4)) %>%
  group_by(cell, season) %>%
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

file_output = sprintf("output_data/residence_time/tif/RES_%s_residence_time_season.tif", res_filename)
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

#==================================================================
# 5) N SEALS PER PIXEL BY SEASON 
#==================================================================

nseals_season <- tsp_tot %>%
  mutate(season = case_when(month <= 2 ~ 1,
                            month >= 3 & month <= 5 ~ 2,
                            month >= 6 & month <= 8 ~ 3,#___ 1 = "Summer", 2 = "Autumn", 3 = "Winter", 4 = "Spring" 
                            .default = 4)) %>%
  group_by(cell, season) %>%
  dplyr::select(c(n_seals, cell, season)) %>%
  filter(row_number() == 1) %>%
  reframe(tot_n_seals = sum(n_seals))

xyCell = xyFromCell(grid, nseals_season$cell)
nseals_season <- nseals_season %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(nseals_season) <- c("lon", "lat")

## Construct raster layer brick for each month
seasons <- unique(nseals_season$season)
r_new <- list()

for (m in seasons) {
  nseals_s <- nseals_season[nseals_season$season == m, ]
  
  lyr <- rasterize(nseals_s, grid, "tot_n_seals")

  r_new[[m]] <- lyr
}

r <- do.call(brick, r_new)
names(r) <- c("Summer", "Autumn", "Winter", "Spring")

file_output = sprintf("output_data/nseals/tif/RES_%s_nseals_sum_pixel_season.tif", res_filename)
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

#==================================================================
# 6) N SEALS PER PIXEL BY MONTH 
#==================================================================

nseals_month <- tsp_tot %>%
  group_by(cell, month) %>%
  dplyr::select(c(n_seals, cell, month)) %>%
  filter(row_number() == 1) %>%
  reframe(tot_n_seals = sum(n_seals))

xyCell = xyFromCell(grid, nseals_month$cell)
nseals_month <- nseals_month %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(nseals_month) <- c("lon", "lat")

## Construct raster layer brick for each month
months <- unique(nseals_month$month)
r_new <- list()

for (m in months) {
  nseals_m <- nseals_month[nseals_month$month == m, ]
  
  lyr <- rasterize(nseals_m, grid, "tot_n_seals")
  
  r_new[[m]] <- lyr
}

r <- do.call(brick, r_new)
names(r) <- month.name

file_output = sprintf("output_data/nseals/tif/RES_%s_nseals_sum_pixel_months.tif", res_filename)
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

#==================================================================
# 6) N SEALS PER PIXEL BY MONTH IN A YEAR
#==================================================================

nseals_month <- tsp_tot %>%
  filter(year == 2012) %>%
  group_by(cell, month, n_seals) %>%
  filter(row_number() == 1) #%>%
  # select(c(cell, month, n_seals))

xyCell = xyFromCell(grid, nseals_month$cell)
nseals_month <- nseals_month %>%
  bind_cols(data_frame("lon" = xyCell[,1], "lat" = xyCell[,2]))

coordinates(nseals_month) <- c("lon", "lat")

## Construct raster layer brick for each month
months <- 1:12
r_new <- list()

for (m in months) {
  nseals_m <- nseals_month[nseals_month$month == m, ]
  if (nrow(nseals_m) > 0) {
    lyr <- rasterize(nseals_m, grid, "n_seals")
  } else {
    lyr <- grid
    lyr[is.na(lyr)] = -999
  }
  r_new[[m]] <- lyr
}

r <- do.call(brick, r_new)
names(r) <- month.name[months]

file_output = sprintf("output_data/nseals/tif/RES_%s_nseals_sum_pixel_months_2012.tif", res_filename)
r2 <- rast(r)
terra::writeRaster(r2, file_output, overwrite = T)

## End script
rm(list=ls())
