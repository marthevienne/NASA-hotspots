##
## Script name: prep_data_ARGOS_bssm
##
## Purpose of script: Prepare tracking data for SSM according 
##                    to Ian Jonsen's packages criteria : SSM {id, date, lc, lon, lat}
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

## Data input
data <- readRDS("~/Desktop/WHOI/Data/behavioural_data/compiled_diag_rm_dupli_non_filtered_tracks")

## Remove LQ-9 locations
data <- data[data$LQ != -9, ]

## Rename LQ locations as described in ARGOS 
data$LQ[data$LQ == -2] = "B"
data$LQ[data$LQ == -1] = "A"

## Create a df with var for SSM (id, date, lc, lon & lat)
format.date <- "%d-%b-%Y %H:%M:%S"
N <- nrow(data)

diag_tab <- matrix(nrow = N, ncol = 5) 
diag_tab <- as.data.frame(diag_tab)
colnames(diag_tab) <- c("id", "date", "lc", "lon", "lat")
diag_tab$id <- as.character(data$REF)
diag_tab$date <- as.POSIXct(data$D_DATE, format = format.date)
diag_tab$lc <- as.character(data$LQ)
diag_tab$lon <- data$LON
diag_tab$lat <- data$LAT

diag_tab <- diag_tab[!is.na(diag_tab$lc),]

## Save as R object and CSV
file_output_diag = "~/Desktop/WHOI/Data/bssm/tracks_for_bssm"
saveRDS(diag_tab, file_output_diag)

## End script
rm(list=ls())
