##
## Script name: OBS_format_polynya_contours.R
##
## Purpose of script: Compile and reformat polynya polygons after Matlab preprocessing
##                    1) Transform CSV files containing all contours (2004-2019) by month into one dataframe
##                    2) Transform CSV files containing the biggest contour for each polynya by month
##                       into one dataframe
##                    3) Compute the union of all polygons from (1) by month and by polynya. Result in dataframe.
##
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-30
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
## Working directory
setwd("~/Desktop/WHOI/Data/polynyas_contours/OBS/")
## ---------------------------
## Library
library(dplyr)
library(ggplot2)
library(lubridate)
## ---------------------------
rm(list=ls())

#==========================================================================================
# 1) MULTIPLE POLYNYA CONTOURS (e.g. multiple contours per polynya, per year and per month)
#==========================================================================================

## Monthly contours (biggest contour per polynya on all 12 months)
files = list.files(path = "all_contours_per_month", pattern = "polynya_contours.csv", full.names = T)
length(files)

## Compile all polynyas contours in dataframe 
polynya_df = matrix(NA, nrow = 0, ncol = 6)
polynya_df = as.data.frame(polynya_df)
colnames(polynya_df) = c("ID", "num", "lon", "lat", "year", "month")
for (file in files) {
  n_month = sub("_.*", "", file)  
  pol = read.csv(file)
  colnames(pol) = c("ID", "num", "lon", "lat", "year")
  pol <- pol %>%
    filter(!(is.na(lat))) %>%
    mutate(month = month.name[as.numeric(n_month)])
  polynya_df = rbind(polynya_df, pol)
}

## Write csv
write.csv(polynya_df, "all_contours_per_month/OBS_multiple_polynyas_contours_df.csv", row.names = F)

#===============================================================================================
# 2) BIGGEST POLYNYA CONTOUR (e.g. one contour per polynya per month (biggest in all the years))
#===============================================================================================
rm(list=ls())

## Monthly contours (biggest contour per polynya on all 12 months)
files = list.files(path = "biggest_contours_per_month", pattern = "polynya_contours.csv", full.names = T)
length(files)

## Compile all polynyas contours in dataframe 
polynya_df = matrix(NA, nrow = 0, ncol = 5)
polynya_df = as.data.frame(polynya_df)
colnames(polynya_df) = c("ID", "lon", "lat", "num", "month")
for (file in files) {
  n_month = sub("_.*", "", file)  
  pol = read.csv(file)
  colnames(pol) = c("ID", "lon", "lat", "num")
  pol <- pol %>%
    filter(!(is.na(lat))) %>%
    mutate(month = month.name[as.numeric(n_month)])
  polynya_df = rbind(polynya_df, pol)
}

## Write csv
write.csv(polynya_df, "biggest_contours_per_month/OBS_biggest_polynyas_contours_df.csv", row.names = F)

## Some polynyas overlap (Utiskkar Bay and Darnley)
pol_sample <- polynya_df %>%
  filter(ID == 11 | ID == 12)

ggplot() +
  geom_polygon(data = pol_sample, aes(x = lon, y = lat, group = interaction(ID, num), col = as.factor(ID)), fill = NA) +
  facet_wrap(~month)


#==================================================================
# 3) Compile the polygon union on all years per month, per polynya 
#==================================================================
rm(list=ls())

## Working directory
setwd("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/")

polynyas = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

months = month.name
pol_ids = unique(polynyas$ID)

pol_df_final = as.data.frame(matrix(NA, nrow = 0, ncol = 5))
colnames(pol_df_final) = c("lon", "lat", "ID", "month", "num")
for (id in pol_ids) {
  for (m in months) {
    pol_sub = subset(polynyas, month == m & ID == id)
    pol_sub$new_ID = paste0(pol_sub$month,".", pol_sub$ID, ".", pol_sub$num)
    keep_num_pol = pol_sub %>% count(new_ID) %>%
      filter(n > 3)
    keep_pol = subset(pol_sub, new_ID %in% keep_num_pol$new_ID)
    pol_sf <-  sfheaders::sf_polygon(
      obj = keep_pol
      , x = "lon"
      , y = "lat"
      , polygon_id = "new_ID"
    )
    
    union <- st_union(pol_sf) %>% 
      st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    n_cont = length(union[[1]])
    num = 1
    for (cont in 1:n_cont) {
      coords = union[[1]][[cont]]
      if (is.array(coords)) {
        pol_df_inter = as.data.frame(coords)
        colnames(pol_df_inter) = c("lon", "lat")
        pol_df_inter$ID = id
        pol_df_inter$month = m
        pol_df_inter$num = num
        pol_df_final = rbind(pol_df_final, pol_df_inter)
        num = num + 1
      } else {
        for (cont2 in 1:length(coords)) {
          coords = union[[1]][[cont]][[cont2]]
          pol_df_inter = as.data.frame(coords)
          colnames(pol_df_inter) = c("lon", "lat")
          pol_df_inter$ID = id
          pol_df_inter$month = m
          pol_df_inter$num = num
          pol_df_final = rbind(pol_df_final, pol_df_inter)
          num = num + 1
        }
      }
    }
  }
}

## Write csv
write.csv(pol_df_final, "OBS_union_polynyas_contours_df.csv", row.names = F)


## TEST BIGGEST CONTOUR
rm(list=ls())

library(geometry)
polynyas = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

months = month.name
pol_ids = unique(polynyas$ID)

pol_max_df <- NULL
for (id in pol_ids) {
  for (m in months) {
  pol_sub = subset(polynyas, month == m & ID == id)
  pol_sub$new_ID = paste0(pol_sub$month,".", pol_sub$ID, ".", pol_sub$num)
  
  # ggplot()+
  #   geom_polygon(data= pol_sub, aes(x = lon, y = lat, group = new_ID, col = new_ID), fill = "NA") +
  #   facet_grid(~month)+
  #   theme(legend.position = "none")
   
  ## keep biggest contours by month
  years = unique(pol_sub$year)
  
  max_area = -1
  for (yr in years) {
    pol_yr = subset(pol_sub, year == yr)
    id_cont = unique(pol_yr$new_ID)
    area <- 0
    for (cont in id_cont) {
      pol_ind <- subset(pol_yr, new_ID == cont)
      area_id <- polyarea(pol_ind$lon, pol_ind$lat)
      # print(plot(pol_ind$lon, pol_ind$lat, type = "l"))
      area <- area + area_id
    }
    if (area > max_area) {
      max_yr <- yr
      max_area <- area
    }
  }
  
  pol_max = subset(pol_sub, year == max_yr)
  
  pol_max_df <- rbind(pol_max_df, pol_max)
  ggplot()+
    geom_polygon(data = pol_max, aes(x = lon, y = lat, group = new_ID), col = "black", fill = "NA") +
    theme(legend.position = "none")
  }
}

write.csv(pol_max_df, "OBS_big_polynyas_contours_df.csv", row.names = F)

## End script
rm(list=ls())



