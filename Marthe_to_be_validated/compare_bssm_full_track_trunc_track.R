# Test run bssm on truncated track (below -58.5°N)
rm(list=ls())

north_boundary = -58.5
id_seal = "ct34-2449-08"

setwd("~/Desktop/WHOI/Data/")

bssm <- readRDS("tracks_for_bssm")
bssm$date <- as.POSIXct(bssm$date,origin="1970-01-01",tz="GMT")
str(bssm)

bssm_raw = subset(bssm, id == id_seal)

bssm_below_NB = subset(bssm_raw, subset = lat < north_boundary)

plot(bssm_raw$date, bssm_raw$lat, type = "l")
abline(h = north_boundary)
points(bssm_below_NB$date, bssm_below_NB$lat, pch = 16, col = "red")

## Full track
library(aniMotum)
fit_FT <- fit_ssm(bssm_raw, time.step = 4, model = "crw", vmax = 4, control = ssm_control(verbose = 0)) # à comprendre
fmp_FT <- fit_mpm(fit_FT, what = "fitted", model = "mpm", control = mpm_control(verbose = 0)) # à comprendre 
this.fit_FT <- grab(fit_FT, what = "predicted", as_sf = FALSE)

## Truncated track
fit_TT <- fit_ssm(bssm_below_NB, time.step = 4, model = "crw", vmax = 4, control = ssm_control(verbose = 0)) # à comprendre
fmp_TT <- fit_mpm(fit_TT, what = "fitted", model = "mpm", control = mpm_control(verbose = 0)) # à comprendre 
this.fit_TT <- grab(fit_TT, what = "predicted", as_sf = FALSE)

## Compare
length(this.fit_TT$date)
length(this.fit_FT$date)

comparable_positions = this.fit_FT[which(this.fit_FT$date %in% this.fit_TT$date),]

plot(comparable_positions$date, comparable_positions$lat, type = "l")
abline(h = north_boundary)
points(this.fit_TT$date, this.fit_TT$lat, pch = 16, col = "red")

plot(comparable_positions$lon - this.fit_TT$lon, comparable_positions$lat - this.fit_TT$lat)
hist(comparable_positions$lon - this.fit_TT$lon, breaks = 30)
hist(comparable_positions$lat - this.fit_TT$lat, breaks = 30)
