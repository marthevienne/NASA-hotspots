##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-16
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
setwd("~/Desktop/WHOI/Data/")

## Set output directory
path_output = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")

## Read netcdf data
file1 <- "~/Dropbox/data/polynya_contours_NCAR_2023/ssmi_cdr_typical_polynya_mask_85%thresh"
file2 <- "~/Dropbox/data/polynya_contours_NCAR_2023/jra55_typical_polynya_mask_hi_0.4mthresh"

nc_typical_polynyas_ssmi <- nc_open(paste0(file1, ".nc"))
nc_typical_polynyas_cesm <- nc_open(paste0(file2, ".nc"))

## Save the print(nc) dump to a text file
for (nc_file in c(file1, file2)) {
  sink(paste0(nc_file, "_metadata.txt"))
  print(nc_open(paste0(nc_file, ".nc")))
  sink()
}

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

## IDENTIFY POLYNYAS
lon = lonSSMI_ctr180_360
lat = latSSMI
poly_matrix = polynya_typical_ssmi_rearranged_360

poly_matrix[which(is.na(poly_matrix))] = 0
poly_matrix[which(poly_matrix != 0)] = NA

image(poly_matrix)

## Check if polynya cut in half in raster
if (length(which(is.na(poly_matrix[,1]) == T)) == 0) {
  print("No polynya cut in half")
} else {
  print("Polynya cut in half...")
}

rotate1 <- t(poly_matrix)
rotate2 <- rotate1[nrow(rotate1):1,]

image(rotate2)

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

saveRDS(r, "raster_polynyas_id_SSMI") # polynyas as raster

## TRANSFORM TO LIST WITH MATRIX, LON (-180->180), LAT
m <- matrix(values(r), ncol = ncol(poly_matrix), nrow = nrow(poly_matrix))
image(m)
rotate1 <- m[, ncol(m):1]
image(rotate1)

id_pol_matrix = rotate1
lonSSMI_new = sapply(lon, function(x) ifelse(x > 180, x - 360, x))

image(lon, lat, id_pol_matrix)

SSMI_id_list = list("pol" = id_pol_matrix, "lon" = lonSSMI_new, "lat" = lat)
saveRDS(SSMI_id_list, "matrix_polynyas_id_SSMI")


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


## IDENTIFY POLYNYAS
lon = lonCESM_ctr180_360
lat = latCESM
poly_matrix = polynya_typical_cesm_rearranged_360

poly_matrix[which(is.na(poly_matrix))] = 0
poly_matrix[which(poly_matrix != 0)] = NA

image(poly_matrix)

## Check if polynya cut in half in raster
if (length(which(is.na(poly_matrix[,1]) == T)) == 0) {
  print("No polynya cut in half")
} else {
  print("Polynya cut in half...")
}

rotate1 <- t(poly_matrix)
rotate2 <- rotate1[nrow(rotate1):1,]

image(rotate2)

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

saveRDS(r, "raster_polynyas_id_CESM") # polynyas as raster

## TRANSFORM TO LIST WITH MATRIX, LON (-180->180), LAT
m <- matrix(values(r), ncol = ncol(poly_matrix), nrow = nrow(poly_matrix))
image(m)
rotate1 <- m[, ncol(m):1]
image(rotate1)

id_pol_matrix = rotate1
lonCESM_new = sapply(lon, function(x) ifelse(x > 180, x - 360, x))

image(lon, lat, id_pol_matrix)

CESM_id_list = list("pol" = id_pol_matrix, "lon" = lonCESM_new, "lat" = lat)
saveRDS(CESM_id_list, "matrix_polynyas_id_CESM")

## TRANSFORM TO DATAFRAME WITH, LON, LAT, ID (NA if no polynya)
r = readRDS("raster_polynyas_id_CESM")
CESM_df <- as.data.frame(r, xy = TRUE) %>% 
  na.omit()

colnames(CESM_df) = c("lon", "lat", "poly_id")
head(CESM_df)

CESM_df$lon = sapply(CESM_df$lon, function(x) ifelse(x > 180, x - 360, x))

ggplot() +
  geom_tile(data = CESM_df, aes(lon, lat, fill = as.factor(poly_id)))

saveRDS(CESM_df, "df_polynyas_id_CESM")

#######
dives = readRDS("~/Desktop/WHOI/Data/interpoled_locations_dives_north_boundary_5meters")



ggplot() +
  geom_tile(data = CESM_df, aes(lon, lat, fill = as.factor(poly_id)))+
  geom_point(dives[1:10000,], mapping = aes(x = lon, y = lat), size = .1)+
  scale_fill_manual()

#######

#==================================================================
# ASSIGN POLYNYA TO INTERPOLATED DIVES
#==================================================================

CESM_id_list = readRDS("matrix_polynyas_id_CESM")
m = CESM_id_list$pol
lon = CESM_id_list$lon
lat = CESM_id_list$lat

range(m, na.rm = TRUE) # range id polynyas

dives = readRDS("~/Desktop/WHOI/Data/interpoled_locations_dives_north_boundary_5meters")
{
  t1 = Sys.time()
  dives$polynya_id_CESM = apply(dives, 1, function(dive)
    get_polynya_id_typical(lon, lat, m, dive[25], dive[26]))
  t2 = Sys.time()
}

ggplot() +
  geom_tile(data = CESM_df, aes(lon, lat, fill = as.factor(poly_id)))+
  #geom_point(dives[1:10000,], mapping = aes(x = lon, y = lat), size = .1)+
  scale_fill_manual(values = pal)

t2-t1
#difftime(t2, t1, units = "min") / nrow(dives_bis) * nrow(dives) # 15 min

saveRDS(dives, "summary_dives_id_pol")

##############
dives_ind = subset(dives, subset = REF == dives$REF[1])

ggplot() +
  geom_point(dives_ind, mapping = aes(x = lon, y = lat, col = as.factor(in_polynya)), size = .1)
