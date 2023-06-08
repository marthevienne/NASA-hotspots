##
## Script name: OBS_assign_polynya_id_to_dive.R
##
## Purpose of script: Assign a polynya id to each dive (or 0 if dive does not occur in a polynya)
##                    from polynya contours given by Esther Portela
##                    => add MONTH and POL variables to dive metrics table
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
library(sp)
library(dplyr)

## Path input data
path_input = "~/Desktop/WHOI/Data/polynyas_contours/"

## Path output data
path_output = "~/Desktop/WHOI/Data/output_data/"

#==================================================================
# ASSIGN A POLYNYA ID TO EACH DIVE 
#==================================================================

polynyas = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/OBS_polynyas_contours_df.csv", header = T) #___polynyas coordinates dataframe
dives = readRDS("~/Desktop/WHOI/Data/output_data/interpoled_locations_dives_north_boundary_5meters") #___dive metrics table (with locations)
dives$month = sapply(dives$DE_DATE, function(t) month.name[month(t)]) #___month at which the dive occur
dives$pol = NA

str(dives)

## Method: overlap between SpatialPoint and SpatialPolygons
months = unique(dives$month)
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

ndives_outside = length(which(is.na(dives$pol))) # number of dives in polynyas
ndives_outside / nrow(dives) * 100 # fraction of dives outside polynyas

pol_summary_usage <- dives %>% 
  group_by(month, pol) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()

pol_summary_usage$month = factor(pol_summary_usage$month, levels = month.name)
ggplot(pol_summary_usage, aes(x = factor(pol), fill = factor(month), group = factor(month))) + 
  geom_bar(aes(y = n_dives), stat = "identity", position = "dodge")

dives$pol[is.na(dives$pol)] = 0

## Save updated dive metrics table
saveRDS(dives, paste0(path_output, "dive_metrics_V3"))

###########################################

### Test method
# Mock dives
dives = data.frame(
  "lat" = c(-69, -77, -63.72815, -68.3),
  "lon" = c(-2, 200,  -56.31249, 149.2),
  "date" = c(
    as.POSIXct("03/09/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/05/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/07/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/05/2020", format = "%d/%m/%Y")
  )
)
dives$month = sapply(dives$date, function(t) month.name[month(t)])
months = unique(dives$month)
dives$pol = NA
print(dives)

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

print(dives)

## End of script
rm(list=ls())
