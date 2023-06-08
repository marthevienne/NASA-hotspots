##
## Script name: run_bssm_overall_tracks
##
## Purpose of script: Run bssm (behavioral state-space model) on each track (separately)
##
## Author: Lucie Bourreau
## Modified by: Marthe Vienne
##
## Date Created: 2022
## Date Modified: 2023-04-26
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
setwd("~/Desktop/WHOI/Data/")

## Library
#install.packages("aniMotum", 
#                 repos = c("https://cloud.r-project.org",
#                           "https://ianjonsen.r-universe.dev"),
#                 dependencies = TRUE)
library(aniMotum) # careful about R version compatibility
library(ggplot2)
library(tibble)

## Source a modified version of Ian Jonsen's quality control plot function
## original at:
## https://github.com/SCAR/RAATD/blob/master/R/duckConfit/R/qc_plot.r
# source("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/codes/plotFun.R")

## Plot figures?
plot_fig = FALSE

## Import data
data <- readRDS("bssm_SES_polynia")
data$date <- as.POSIXct(data$date, origin="1970-01-01", tz="GMT")
str(data)

## Fit SSM
fit <- fit_ssm(as_tibble(data), time.step = 4, model = "crw", vmax = 4, control = ssm_control(verbose = 0)) # à comprendre
fmp <- fit_mpm(fit, what = "fitted", model = "mpm", control = mpm_control(verbose = 0)) # à comprendre 

## Write fit as R object
saveRDS(fit, "fit_SSM_all_tracks")
write.csv(fit, "fit_SSM_all_tracks.csv", row.names = F)

if (plot_fig) {
  crs = "+proj=stere +lon_0=68 +datum=WGS84 +units=km"
  map_tracks(fit, crs)
}


## Useful functions ####################################
map_tracks <- function(fit, crs = NULL) {
  my.aes = aes_lst(conf = FALSE, id_pal = "Viridis")
  
  map(fit,  what = "predicted", aes = my.aes, crs = crs) + 
    theme(legend.position ="none")
}

