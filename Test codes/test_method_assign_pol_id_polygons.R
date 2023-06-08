rm(list = ls())

## Library
library(sp)
library(dplyr)

##=== Functions =====================================
get_polynya_id <- function(lon, lat, pol) {
  in_pol = point.in.polygon(lon, lat, pol$lon, pol$lat)
  res = in_pol * unique(pol$ID)
  if (res == 0) {
    return(NA)
  }
  return(res)
}
##====================================================

## Path input data
path_input = "~/Desktop/WHOI/Data/polynyas_contours/"

#==================================================================
# ASSIGN A POLYNYA ID TO EACH DIVE 
#==================================================================
polynyas = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/OBS_polynyas_contours_df.csv", header = T)
dives = readRDS("~/Desktop/WHOI/Data/output_data/interpoled_locations_dives_north_boundary_5meters")
dives$month = sapply(dives$DE_DATE, function(t) month.name[month(t)])
months = unique(dives$month)
dives$pol = NA

colnames(dives)
lat_i = which(colnames(dives) == "lat")
lon_i = which(colnames(dives) == "lon")
pol_i = which(colnames(dives) == "pol")
head(dives)

## Method 1: is dive location in a polynya? If yes, then get polynya id
## VERY LONG METHOD FOR BIG DATASET
for (m in months) {
  print(m)
  row_dives_month = which(dives$month == m)
  polynyas_month = subset(polynyas, subset = month == m)
  poly_ID = unique(polynyas_month$ID)

  for (id in poly_ID) {
    print(id)
    pol = polynyas_month[polynyas_month$ID == id,]
    dives$pol[row_dives_month] = apply(dives[row_dives_month, ], 1, function(dive) ifelse(is.na(dive[pol_i]),
                                                                                          get_polynya_id(dive[lon_i], dive[lat_i], pol),
                                                                                          dive[pol_i]))
  }
}


## Method 2: overlap between SpatialPoint and SpatialPolygons
for (m in months) {
  print(m)
  row_dives_m = which(dives$month == m)
  polynyas_m = subset(polynyas, subset = month == m)
  poly_ID = unique(polynyas_m$ID)
  
  dives_m = dives[row_dives_m, ]
  val = data.frame("val" = rep(1, nrow(dives_m)))
  sp_dives = SpatialPointsDataFrame(cbind(dives_m$lon, dives_m$lat), data = val)
  
  list_pol_m = split(polynyas_m, f = polynyas_m$ID)
  list_pol_m <- lapply(list_pol_m, function(x) { x["ID"] <- NULL; x })
  list_pol_m <- lapply(list_pol_m, function(x) { x["month"] <- NULL; x })
  
  p_pol <- lapply(list_pol_m, Polygon)
  ps_pol <- lapply(seq_along(p_pol), function(i) Polygons(list(p_pol[[i]]), ID = names(p_pol)[i]))
  sps_pol <- SpatialPolygons(ps_pol)
  
  id = over(sp_dives, sps_pol)
  dives$pol[row_dives_m] = as.numeric(id)
}