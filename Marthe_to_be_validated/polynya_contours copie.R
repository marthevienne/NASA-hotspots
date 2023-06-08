##
## Script name: polynya_contours
##
## Purpose of script: Assign a polynya id to each dive (or 0 if dive does not occur in a polynya)
##                    as defined by NCAR (proxy = sea-ice concentration)
##
## Author: Marthe Vienne
##
## Date Created: 2023-04-25
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
rm(list = ls())

## Library
library(ncdf4) # for netcdf manipulation
library(ggplot2)
library(lubridate)
library(wesanderson)
library(plotly)
library(raster)

## Functions
source("~/Desktop/WHOI/Codes/functions_polynyas.R")

## Set working directory
setwd("~/Dropbox/data/polynya_contours_NCAR_2023/")

## Set output directory
path_output = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")

## Read netcdf data
file1 <- "ssmi_cdr_typical_polynya_mask_85%thresh"
file2 <- "jra55_typical_polynya_mask_hi_0.4mthresh"
#file3 <- "SSMI.CDR.85%thresh.polynya2_sh.197901-202012"

nc_typical_polynyas_ssmi <- nc_open(paste0(file1, ".nc"))
nc_typical_polynyas_cesm <- nc_open(paste0(file2, ".nc"))
#nc_monthly_polynyas <- nc_open(paste0(file2, ".nc"))


## Save the print(nc) dump to a text file
for (nc_file in c(file1, file2)) {
  sink(paste0(nc_file, "_metadata.txt"))
  print(nc_open(paste0(nc_file, ".nc")))
  sink()
}


## Get variables : lon, lat, time, 3D matrix (polynyas ids and types raster through time (1979-01))
# lon <- ncvar_get(nc_monthly_polynyas, varid = "tlon1d")
# lat <- ncvar_get(nc_monthly_polynyas, "tlat1d", verbose = F)
# polynya_matrix_id <- ncvar_get(nc_monthly_polynyas, "polynya_ID")
# polynya_matrix_type <- ncvar_get(nc_monthly_polynyas, "polynyas")
# time = seq(as.Date("1979/1/1"), as.Date("2020/12/1"), "month")
# polyID = ncvar_get(nc_monthly_polynyas, "polyID")


#==================================================================
# SSMI contours 
#==================================================================

## Get variables for typical polynyas (snapshot): lon, lat, time, 3D matrix of polynyas)
lonSSMI <- ncvar_get(nc_typical_polynyas_ssmi, varid = "TLONG_sub")[,1]
latSSMI <- ncvar_get(nc_typical_polynyas_ssmi, varid = "TLAT_sub")[1,]
polynya_typical_all_ssmi <-
  ncvar_get(nc_typical_polynyas_ssmi, "polynya_typical_all") # code = 0 (other) or 1 (polynya)
polynya_typical_winter_ssmi <-
  ncvar_get(nc_typical_polynyas_ssmi, "polynya_typical_winter") # code = 0 (other) or 1 (polynya)


## Grid resolution
(max(latSSMI) - min(latSSMI)) / length(latSSMI)
(max(lonSSMI) - min(lonSSMI)) / length(lonSSMI)

## Rearrange matrix to lon -180 -> 180 centered on 0
lonSSMI_180 = sapply(lonSSMI, function(lon) ifelse(lon > 180, lon - 360, lon))
index_start_lon180 = which(lonSSMI_180 == min(lonSSMI_180))
seq_lon_180 = c(seq(index_start_lon180, nrow(polynya_typical_all_ssmi)), 
                1:(index_start_lon180 - 1))
lonSSMI_ctr0_180 = lonSSMI_180[seq_lon_180]
polynya_typical_ssmi_rearranged_180 = polynya_typical_all_ssmi[seq_lon_180, ]

## Rearrange matrix to lon 0 -> 360 centered on 180
index_start_lon360 = which(lonSSMI == min(lonSSMI))
seq_lon_360 = c(seq(index_start_lon360, nrow(polynya_typical_all_ssmi)), 
                1:(index_start_lon360 - 1))
lonSSMI_ctr180_360 = lonSSMI[seq_lon_360]
polynya_typical_ssmi_rearranged_360 = polynya_typical_all_ssmi[seq_lon_360, ]
image(polynya_typical_ssmi_rearranged_360)


## Map typical contours polynyas
pdf(
  paste0(path_output, "SSMI_typical_contours_polynyas.pdf"),
  height = 5,
  width = 12
)
print(image(lonSSMI_ctr180_360, latSSMI, polynya_typical_ssmi_rearranged_360, 
            ylim = c(-80, -60), xlab = "Lon (°E)", ylab = "Lat (°N)"))
print(title("Typical polynyas (SSMI) over 1979-2020 period \n(cells identified as polynyas > 10% of the time)"))
dev.off()


#==================================================================
# CESM contours 
#==================================================================

## Get variables for typical polynyas (snapshot): lon, lat, time, 3D matrix of polynyas)
lonCESM <- ncvar_get(nc_typical_polynyas_cesm, varid = "TLONG_sub")[,1]
latCESM <- ncvar_get(nc_typical_polynyas_cesm, varid = "TLAT_sub")[1,]
polynya_typical_all_cesm <-
  ncvar_get(nc_typical_polynyas_cesm, "polynya_typical_all") # code = 0 (other) or 1 (polynya)
polynya_typical_winter_cesm <-
  ncvar_get(nc_typical_polynyas_cesm, "polynya_typical_winter") # code = 0 (other) or 1 (polynya)

## Grid resolution
(max(latCESM) - min(latCESM)) / length(latCESM)
(max(lonCESM) - min(lonCESM)) / length(lonCESM)

## Rearrange matrix to lon -180 -> 180 centered on 0
lonCESM_180 = sapply(lonCESM, function(lon) ifelse(lon > 180, lon - 360, lon))
index_start_lon180 = which(lonCESM_180 == min(lonCESM_180))
seq_lon_180 = c(seq(index_start_lon180, nrow(polynya_typical_all_cesm)), 
                1:(index_start_lon180 - 1))
lonCESM_ctr0_180 = lonCESM_180[seq_lon_180]
polynya_typical_cesm_rearranged_180 = polynya_typical_all_cesm[seq_lon_180, ]

## Rearrange matrix to lon 0 -> 360 centered on 180
index_start_lon360 = which(lonCESM == min(lonCESM))
seq_lon_360 = c(seq(index_start_lon360, nrow(polynya_typical_all_cesm)), 
                1:(index_start_lon360 - 1))
lonCESM_ctr180_360 = lonCESM[seq_lon_360]
polynya_typical_cesm_rearranged_360 = polynya_typical_all_cesm[seq_lon_360, ]
image(polynya_typical_cesm_rearranged_360)


## Map typical contours polynyas
pdf(
  paste0(path_output, "CESM_typical_contours_polynyas.pdf"),
  height = 5,
  width = 12
)
print(image(lonCESM_ctr180_360, latCESM, polynya_typical_cesm_rearranged_360,
            ylim = c(-80, -60), xlab = "Lon (°E)", ylab = "Lat (°N)"))
print(title("Typical polynyas (CESM) over 1979-2020 period \n(cells identified as polynyas > 10% of the time)"))
dev.off()


## Test: assign pol to dive
# A dive is defined by date, lon, lat

# Mock dives
dives = data.frame(
  "lat" = c(-63.72815, -63.72815, -63.72815),
  "lon" = c(-56.31249, -56.31249,  -56.31249),
  "date" = c(
    as.POSIXct("03/09/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/05/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/07/1979", format = "%d/%m/%Y")
  ),
  "id" = c(1, 2, 3)
)


#id_polynya_typical = readRDS("polynya_id_typical_all_map")
lat = id_polynya_typical[["lat"]]
lon = id_polynya_typical[["lon"]]

lon[which(lon > 180)] = lon[which(lon > 180)] - 360
plot(lon)

dives$pol = apply(dives, 1, function(dive)
  get_polynya_id_typical(lon, lat, id_polynya_typical, dive[2], dive[1]))

dives = readRDS("~/Desktop/WHOI/Data/interpoled_locations_dives_north_boundary_5meters")
dives_bis = subset(dives[1:10000,], select = c(lon, lat))

#id_pol_matrix = polynya_typical_cesm_rearranged_180
range(id_pol_matrix, na.rm = TRUE)
lon = lonCESM_ctr0_180
lat = latCESM

dives = readRDS("~/Desktop/WHOI/Data/interpoled_locations_dives_north_boundary_5meters")
dives = subset(dives, select = c(REF, NUM, lon, lat))
{
t1 = Sys.time()
dives$pol = apply(dives, 1, function(dive)
  get_polynya_id_typical(lon, lat, id_pol_matrix, dive[3], dive[4]))
t2 = Sys.time()
}

t2-t1
#difftime(t2, t1, units = "min") / nrow(dives_bis) * nrow(dives) # 15 min

# Test: polynyas map in East Antarctica for each dive date (by type = coastal, open-ocean)
lonlim = c(0, 160)
latlim = c(-75,-58.8)

for (date in dives$date) {
  date <- as.POSIXct(date, origin = "1970/01/01")
  plot_contour_polynyas_type(polynya_matrix_type, time, date, latlim, lonlim)
}


# Shakleton polynya
# Test: polynyas map in East Antarctica for each dive date (by type = coastal, open-ocean)
latlim = c(-67, -63)
lonlim = c(90, 104)

pdf(
  paste0(path_output, "shackleton_monthly_contours.pdf"),
  height = 5,
  width = 5
)
for (date in time) {
  date <- as.Date(date, origin = "1970/01/01")
  print(plot_contour_polynyas_type(polynya_matrix_type, time, date, latlim, lonlim))
}
dev.off()

pdf(
  paste0(path_output, "shackleton_typical_contour.pdf"),
  height = 5,
  width = 5
)
print(image(lon, lat, polynya_mask_typical_all, xlim = lonlim, ylim = latlim))
dev.off()

# Vincennes Bay and Cape Poinsett
latlim = c(-67, -63)
lonlim = c(106, 116)

for (date in time[1:24]) {
  date <- as.Date(date, origin = "1970/01/01")
  plot_contour_polynyas_type(polynya_matrix_type, time, date, latlim, lonlim)
}

# Cape Darnley
latlim = c(-69, -63)
lonlim = c(63, 73)

for (date in time[1:24]) {
  date <- as.Date(date, origin = "1970/01/01")
  plot_contour_polynyas_type(polynya_matrix_type, time, date, latlim, lonlim)
}


## IDENTIFY POLYNYAS

## CESM
lon = lonCESM_ctr180_360
lat = latCESM
poly_matrix = polynya_typical_cesm_rearranged_360
file_output = "polynyas_polygons_CESM"

## SSMI
lon = lonSSMI_ctr180_360
lat = latSSMI
poly_matrix = polynya_typical_ssmi_rearranged_360
file_output = "polynyas_polygons_SSMI"

#-------------
poly_matrix[which(is.na(poly_matrix))] = 0
poly_matrix[which(poly_matrix != 0)] = NA

image(poly_matrix)

rotate1 <- t(poly_matrix)
rotate2 <- rotate1[nrow(rotate1):1,]

image(rotate2)

## Check if polynya cut in half in raster
if (length(which(is.na(rotate2[,1]) == T)) == 0) {
  print("No polynya cut in half")
} else {
  print("Polynya cut in half...")
}

r = raster(rotate2)
image(r)

id_pol = 0
while (length(which(is.na(values(r) > 0)))) {
  id_pol = id_pol + 1

  same_pol_cells = which(is.na(values(r)))[1]
  r[same_pol_cells] = id_pol

  while (length(same_pol_cells) > 0) {
    adj_cells = adjacent(r, same_pol_cells, directions=8, pairs = FALSE)
    same_pol_cells = adj_cells[which(is.na(values(r)[adj_cells]))]
    r[same_pol_cells] = id_pol
  }
}

r[which(values(r) == 0)] = NA

plot(r)
ext <- extent(min(lon), max(lon), min(lat), max(lat))
extent(r) <- ext
r <- setExtent(r, ext, keepres=TRUE)
plot(r)

saveRDS(r, file_output)

poly_polygons = rasterToPolygons(r, na.rm = T, n = 16, dissolve = F)
n.poly = length(poly_polygons@polygons)
plot(poly_polygons)
poly_polygons@data$id = seq(1:n.poly)

# Transform lon coordinates of polygons in -180->180
df_contours_polynyas = NULL
for (id in poly_polygons@data$id) {
  lon_poly_360 = poly_polygons@polygons[[id]]@Polygons[[1]]@coords[,1]
  lon_poly_180 = sapply(poly_polygons@polygons[[id]]@Polygons[[1]]@coords[,1],
                                                               function(lon)
                                                                 ifelse(lon > 180, lon - 360, lon))
  lat_poly = poly_polygons@polygons[[id]]@Polygons[[1]]@coords[,2]
  id_poly_array = rep(id, length(lat_poly))
  df_contours_polynyas = rbind(df_contours_polynyas, cbind(id_poly_array, lat_poly, lon_poly_180, lon_poly_360))
}

df_contours_polynyas = as.data.frame(df_contours_polynyas)
colnames(df_contours_polynyas) = c("id_poly", "lat", "lon_180", "lon_360")

ggplot(data = df_contours_polynyas, aes(x = lon_360, y = lat, group = id_poly, fill = id_poly)) +
  geom_polygon()

saveRDS(df_contours_polynyas, file_output)


##################
## IDENTIFY POLYNYAS

## CESM
lon = lonCESM_ctr180_360
lat = latCESM
poly_matrix = polynya_typical_cesm_rearranged_360
file_output = "polynyas_polygons_CESM"

## SSMI
lon = lonSSMI_ctr180_360
lat = latSSMI
poly_matrix = polynya_typical_ssmi_rearranged_360
file_output = "polynyas_polygons_SSMI"

#-------------
poly_matrix[which(is.na(poly_matrix))] = 0
poly_matrix[which(poly_matrix != 0)] = NA

image(poly_matrix)

rotate1 <- t(poly_matrix)
rotate2 <- rotate1[nrow(rotate1):1,]

image(rotate2)

## Check if polynya cut in half in raster
if (length(which(is.na(rotate2[,1]) == T)) == 0) {
  print("No polynya cut in half")
} else {
  print("Polynya cut in half...")
}

r = raster(rotate2)
image(r)

id_pol = 0
while (length(which(is.na(values(r) > 0)))) {
  id_pol = id_pol + 1
  
  same_pol_cells = which(is.na(values(r)))[1]
  r[same_pol_cells] = id_pol
  
  while (length(same_pol_cells) > 0) {
    adj_cells = adjacent(r, same_pol_cells, directions=8, pairs = FALSE)
    same_pol_cells = adj_cells[which(is.na(values(r)[adj_cells]))]
    r[same_pol_cells] = id_pol
  }
}

r[which(values(r) == 0)] = NA

m <- matrix(values(r), ncol = ncol(poly_matrix), nrow = nrow(poly_matrix))
image(m)
rotate1 <- m[, ncol(m):1]
image(rotate1)

id_pol_matrix = rotate1

plot(r)
ext <- extent(min(lon), max(lon), min(lat), max(lat))
extent(r) <- ext
r <- setExtent(r, ext, keepres=TRUE)
plot(r)

saveRDS(r, file_output)

poly_polygons = rasterToPolygons(r, na.rm = T, n = 16, dissolve = F)
n.poly = length(poly_polygons@polygons)
plot(poly_polygons)
poly_polygons@data$id = seq(1:n.poly)
