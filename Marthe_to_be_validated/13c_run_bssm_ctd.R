## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-06
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
## ---------------------------
## Library
library(aniMotum) # careful about R version compatibility
library(ggplot2)
library(tibble)
library(dplyr)
## ---------------------------
## Paths
path_input = "~/Desktop/WHOI/Data/output_data/"
path_output = "~/Desktop/WHOI/Data/output_data/"
path_fig <- "~/Dropbox/data/outputs_Marthe_2023/ssm_SES_ctd/"
## ---------------------------
## Functions
## ---------------------------

## Data input
bssm <- readRDS(paste0(path_input, "ctd_stations_ARGOS"))
bssm$date <- as.POSIXct(bssm$date, origin = "1970-01-01", tz = "GMT")
str(bssm)

## Fit the SSM individual by individual

foo <- data.frame()

for (i in 1:length(unique(bssm$id))) {
  ## Individual i
  this.id <- unique(bssm$id)[i]
  print(this.id)
  print(paste0("id: ", i, "/", length(unique(bssm$id))))
  
  ## Subset by individual
  d <- bssm %>%
    filter(id == this.id)
  
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
  
  dev.off()
  
  ## Get the predicted locations
  this.fit <- grab(fit, what = "predicted", as_sf = FALSE)

  foo <- rbind(foo, this.fit)
}


file_pred_SSM = paste0(path_output,  "predictedTracks_ssm_ctd")
saveRDS(foo, file_pred_SSM)


## Track plot
# my.pal = "Viridis" # Common pal along the work -> save in
# 
# ggplot(foo, aes(x = lon, y = lat, colour = id)) +
#   geom_point(size = .2, show.legend = FALSE) +
#   coord_quickmap()


## End script
rm(list=ls())
