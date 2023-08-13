##
## Script name: compile_diag_files
##
## Purpose of script: 1) Remove time duplicates in ARGOS positions for each track 
##                       -> select most accurate position if exists
##                    2) Compile diag files for BSSM (behavioral state-space model)
##                    3) Create seals table with id (REF), start time (start_track), 
##                       end time (end_track) and tracking duration (dur_track)
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
## Notes: Compile all individual files (diag files) in the same table (only the first 6th columns) 
##        -> each file (= tags batch) contains multiple ind (= REF)
##        REF => The first part identifies the batch of tags,
##        the second part is the name or number of the animal, 
##        the third part is the year
##
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
setwd("~/Desktop/NASA-hotspots/")
## ---------------------------
## Library
library(readxl)
library(lubridate)
## ---------------------------
## Paths
path_diag_files = "~/Dropbox/data/diag_dive_ind_pol/"
## ---------------------------
## Functions
source("useful_functions/select_ARGOS_position.R")
## ---------------------------

## Data input
list_diag_files <- list.files(path = path_diag_files, pattern = '_diag.xlsx', recursive = F)

#==================================================================
# 1) REMOVE TIME DUPLICATES IN ARGOS POSITIONS
# 2) COMPILE DIAG FILES FOR BSSM
#==================================================================

data <- NULL
summary <- NULL
start_deploy <- NULL
end_deploy <- NULL
for (file in list_diag_files) {
  print(file)
  deployment <-
    read_excel(paste0(path_diag_files, file))
  names(deployment)[names(deployment) == 'ref'] <- 'REF'
  seals_deploy = unique(deployment$REF)
  for (seal in seals_deploy) {
    data_seal <- deployment[, 1:6][deployment$REF == seal, ]
    start_deploy <- c(start_deploy, data_seal$D_DATE[1])
    end_deploy <- c(end_deploy, data_seal$D_DATE[length(data_seal$D_DATE)])
    dupli_date_seal <- data_seal$D_DATE[duplicated(data_seal$D_DATE)]
    unique_data_seal = subset(data_seal, subset = !D_DATE %in% dupli_date_seal)
    for (date in unique(dupli_date_seal)) {
      unique_data_seal <- rbind(unique_data_seal, select_ARGOS_position(deployment, seal, date))
    }
    summary_seal = cbind("ref" = seal, "n.ARGOS.positions" = nrow(data_seal), "n.dupli.dates" = length(dupli_date_seal))
    summary = rbind(summary, summary_seal)
    unique_data_seal <- unique_data_seal[order(unique_data_seal$D_DATE), ]
    data <- rbind(data, unique_data_seal)
  }
}

## Check if duplicates remain in data
sprintf("%s duplicates remaining", length(which(duplicated(data))))

summary <- as.data.frame(summary)
summary$n.ARGOS.positions <- as.numeric(summary$n.ARGOS.positions)
summary$n.dupli.dates <- as.numeric(summary$n.dupli.dates)

## Save summary
write.csv(summary, file = "~/Desktop/WHOI/Data/summary_outputs/summary_filter_duplicates_diag.csv", row.names = F)

## Reformat data: make sure coordinates are numerical
data$LAT <- gsub(',','.', data$LAT)
data$LON <- gsub(',','.', data$LON)
data$LAT <- as.numeric(data$LAT)
data$LON <- as.numeric(data$LON)

## Add date in yyyy-mm-dd format
data$DATE = format(data$D_DATE, "%Y-%m-%d") #yyyy-mm-dd

## Save data as R object and CSV
#Variables in data: {REF, PTT, D_Date, LQ, LAT, LON, DATE}
file_output_diag =  "~/Desktop/WHOI/Data/behavioural_data/compiled_diag_rm_dupli_non_filtered_tracks"
saveRDS(data, file_output_diag)

#==================================================================
# 3) CREATE SEALS TABLE
#==================================================================

seals = as.data.frame(unique(data$REF))
colnames(seals) = "REF"
seals$start_track = start_deploy
seals$end_track = end_deploy
seals$dur_track = (end_deploy - start_deploy) / 3600 / 24

## Save compiled dive data
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)

## End script
rm(list=ls())

