##
## Script name: OBS_format_polynya_contours.R
##
## Purpose of script: Compile messy data from CSV files (one for each month) into one table 
##                    of polynya contours exported from Matlab
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
rm(list=ls())

## Working directory
setwd("~/Desktop/WHOI/Data/polynyas_contours/OBS/Matlab/")

## Library
library(ggplot2)

## Monthly contours (biggest contour per polynya on all 12 months)
files = list.files(pattern = "polynya_contours.csv")
length(files)

## Compile all polynyas contours in dataframe 
polynya_df = matrix(NA, nrow = 0, ncol = 5)
polynya_df = as.data.frame(polynya_df)
colnames(polynya_df) = c("ID", "lon", "lat", "month", "num")
for (file in files) {
  n_month = sub("_.*", "", file)  
  pol = read.csv(file)
  colnames(pol) = c("ID", "lon", "lat", "num")
  pol = pol[which(!is.na(pol$lat)),] 
  pol$month = month.name[as.numeric(n_month)]
  polynya_df = rbind(polynya_df, pol)
}


## Write csv
write.csv(polynya_df, "OBS_multiple_polynyas_contours_df.csv", row.names = F)

## Some polynyas overlap (Utiskkar Bay and Darnley)
pol_sub = subset(polynya_df, subset = ID == 11 | ID == 12)

ggplot() +
  geom_polygon(data = pol_sub, aes(x = lon, y = lat, group = ID, col = as.factor(ID)), fill = NA) +
  facet_wrap(~month)



pol_sub = subset(polynya_df, subset = ID == 12)
ggplot() +
  geom_polygon(data = pol_sub, aes(x = lon, y = lat, group = num), col = "black", fill = NA)+
  facet_wrap(~month)


ggplot() +
  geom_polygon(data = polynya_df, aes(x = lon, y = lat, group = interaction(ID, num)), col = "black", fill = NA)+
  facet_wrap(~month)

ggplot()


## End script
rm(list=ls())



