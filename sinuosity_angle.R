## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-11
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
setwd("~/Desktop/WHOI/Data/output_data/")
## ---------------------------
## Library
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------
dives <- readRDS("dive_metrics_V8")

bssm <- readRDS("fittedTracks_ssm_behavior") #___dives
head(bssm)

bssm_seal <- bssm %>%
  filter(id == "ct109-939-14")

plot(bssm_seal$date, bssm_seal$g, pch = 16, cex = .4)

dives_seal <- dives %>%
  filter(REF == "ct109-939-14")

library(maptools)

trackAngle <- function(xy) {
  angles <- abs(c(trackAzimuth(xy), 0) -
                  c(0, rev(trackAzimuth(xy[nrow(xy):1, ]))))
  angles <- ifelse(angles > 180, 360 - angles, angles)
  angles[is.na(angles)] <- 180
  angles[-c(1, length(angles))]
}

df_loc <- matrix(cbind(dives_seal$interpLon, dives_seal$interpLat), ncol = 2)
angles <- trackAngle(df_loc)

plot(angles/180, pch = 16, cex = .4)

dives_seal <- dives_seal %>%
  filter((!is.na(interpLat)))

library(argosfilter)
lon <- dives_seal$interpLon
lat <- dives_seal$interpLat
toto = bearingTrack(lat = abs(lat[1:1000]), lon = lon[1:10000])

toto <- NULL
for (i in 2:(length(lon))) {
  toto <- c(toto, my_bearing(lat[i-1], lat[i], lon[i-1], lon[i]))
}
plot(toto/360)

plot(dives_seal$DE_DATE[2:length(dives_seal$DE_DATE)], toto)

## End script
rm(list=ls())
