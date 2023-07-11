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
## ---------------------------
## Paths
path_input = "~/Desktop/WHOI/Data/output_data/"
path_output = "~/Desktop/WHOI/Data/output_data/"
## ---------------------------
## Functions
## ---------------------------

data <- readRDS(paste0(path_input, "ctd_stations_table"))
names(data)

## Remove LQ-9 locations
data <- data[data$PQC != -9, ]

## Rename LQ locations as described in ARGOS 
data$PQC[data$PQC == -2] = "B"
data$PQC[data$PQC == -1] = "A"

format.date <- "%d-%b-%Y %H:%M:%S"

data_ARGOS <- data %>%
  mutate(id = REF,
         date = as.POSIXct(time, format = format.date),
         lc = PQC) %>%
  select(c(id, date, lc, lon, lat))

saveRDS(data_ARGOS, paste0(path_output, "ctd_stations_ARGOS"))

## End script
rm(list=ls())
