##
## Script name: run_bssm
##
## Purpose of script: Run BSSM (behavioral state-space model) on each track (separately)
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
rm(list = ls())

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
## https://github.com/SCAR/RAATD/blob/master/R/duckConfit/R/qc_plot.r# source("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/codes/plotFun.R")


## Data input
bssm <- readRDS("~/Desktop/WHOI/Data/bssm/tracks_for_bssm")
bssm$date <- as.POSIXct(bssm$date, origin = "1970-01-01", tz = "GMT")
str(bssm)

## Fit the SSM individual by individual

foo <- data.frame()
foo2 <- data.frame()

path_fig <- "~/Dropbox/data/outputs_Marthe_2023/ssm_SES/"

for (i in 1:length(unique(bssm$id))) {
  ## Individual i
  this.id <- unique(bssm$id)[i]
  print(this.id)
  print(paste0("id: ", i, "/", length(unique(bssm$id))))
  
  ## Subset by individual
  d <- bssm[bssm$id == this.id,]
  
  ## Check track duration
  dur <- difftime(max(d$date), min(d$date), units = "hours")
  
  ## Fit the SSM
  fit <-
    fit_ssm(
      d,
      time.step = 4,
      model = "crw",
      vmax = 4,
      control = ssm_control(verbose = 0)
    ) # à comprendre
  fmp <-
    fit_mpm(fit,
            what = "fitted",
            model = "mpm",
            control = mpm_control(verbose = 0)) # à comprendre
  
  ## Ian's plots
  pdf(paste0(path_fig, sprintf("id_%s_2.pdf", this.id)),
      height = 10,
      width = 8)
  
  #a ggplot object with either: (type = 1) 1-d time series of fits to data, separated into x and y components
  #(units = km) with prediction uncertainty ribbons (2 x SE); or (type = 2) 2-d fits to data (units = km)
  
  print(plot(
    fit,
    what = c("predicted"),
    type = 1,
    #1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
    outlier = TRUE,
    pages = 1,
    #plots of all individuals on a single page (pages = 1; default) or each individual
    #on a separate page (pages = 0)
    ncol = 1,
    ask = FALSE
  )) #jaune- outlier location dropped by prefilter
  
  print(plot(
    fit,
    what = c("predicted"),
    type = 2,
    #1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
    outlier = TRUE,
    pages = 1,
    #plots of all individuals on a single page (pages = 1; default) or each individual
    #on a separate page (pages = 0)
    ncol = 1,
    ask = FALSE
  )) #jaune- outlier location dropped by prefilter
  
  #print(fmap(fit, what = "predicted", obs = TRUE, size = c(0.5, 0.8))) #when mapping single tracks, should locations be coloured by date (logical; default = FALSE)
  print(plot(fmp, pal = "Cividis", ask = FALSE))
  
  ## using cowplot to add southern elephant seal silhouettes to map
  #print(ggdraw() +
  #        draw_plot(m))
  #+draw_image("Figures/sese_female_orig.png",  x=0.85, y=0.45, scale=0.25, hjust=0.5, vjust=0.5))
  
  print(map(fit, what = "predicted", crs = "+proj=stere +lon_0=69 +units=km +datum=WGS84"))
  
  dev.off()
  
  ## Get the predicted locations
  this.fit <- grab(fit, what = "predicted", as_sf = FALSE)
  this.fit2 <- grab(fmp,  as_sf = FALSE)
  
  foo <- rbind(foo, this.fit)
  foo2 <- rbind(foo2, this.fit2)
}


## Write foo as R object and CSV
file_pred_SSM = "~/Desktop/WHOI/Data/bssm/predictedTracks_ssm"
saveRDS(foo, file_pred_SSM)

file_fitt_BSSM = "~/Desktop/WHOI/Data/bssm/fittedTracks_ssm_behavior"
saveRDS(foo2, file_fitt_BSSM)

## Track plots
# my.pal = "Viridis" # Common pal along the work -> save in
#
# ggplot(foo, aes(x = lon, y = lat, colour = id)) +
#   geom_point(size = .2, show.legend = FALSE) +
#   coord_quickmap()

## End script
rm(list=ls())
