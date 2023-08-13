##
## Script name: compile_dive_files
##
## Purpose of script: 1) Compile dive files
##                    2) Remove duplicated dives
##                    3) Update seals table (add has_dives, n_dives_bf_filt and n_dives_af_rm_dupli)
##
## Author: Lucie Bourreau
## Modified by: Marthe Vienne
##
## Date Created: 2022
## Date Modified: 2023-06-05
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
path_input = "~/Dropbox/data/diag_dive_ind_pol/"
## ---------------------------
## Library
library(dplyr)
library(readxl)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Data input
list_dive_files <-
  list.files(path = path_input,
             pattern = '_dive.xlsx',
             recursive = F)

#==================================================================
# 1) COMPILE DIVE FILES 
#==================================================================

data <- NULL
for (file in list_dive_files) {
  print(file)
  deployment <-
    read_excel(paste0(path_input, file))
  deployment <- rename_with(deployment, toupper)
  deployment <- cbind.data.frame(
    REF = deployment$REF,
    DE_DATE = as.POSIXct(deployment$DE_DATE, origin = "1970-01-01", tz = "GMT"),
    SURF_DUR = deployment$SURF_DUR,
    DIVE_DUR = deployment$DIVE_DUR,
    MAX_DEP = deployment$MAX_DEP,
    D1 = deployment$D1,
    D2 = deployment$D2,
    D3 = deployment$D3,
    D4 = deployment$D4,
    T1 = deployment$T1,
    T2 = deployment$T2,
    T3 = deployment$T3,
    T4 = deployment$T4,
    LAT = deployment$LAT,
    LON = deployment$LON,
    START_LAT = deployment$START_LAT,
    START_LON = deployment$START_LON
  )
  data <- rbind(data, deployment)
}

## Reformat data: make sure coordinates are numerical
data$LAT = as.numeric(gsub(",", ".", data$LAT))
data$LON = as.numeric(gsub(",", ".", data$LON))
data$START_LAT = as.numeric(gsub(",", ".", data$START_LAT))
data$START_LON = as.numeric(gsub(",", ".", data$START_LON))

## Save compiled dive data
file_output =  "~/Desktop/WHOI/Data/behavioural_data/compiled_non_filtered_dives"
saveRDS(data, file_output)

#==================================================================
# 2) REMOVE DUPLICATED DIVES BY SEAL 
#==================================================================

seals = read.csv("~/Desktop/WHOI/Data/seals.csv")

data_seals_unique <- NULL
for (seal in seals$REF) {
  print(seal)
  dives_seal = data[data$REF == seal,]
  unique_dives_seal = unique(dives_seal)
  data_seals_unique <- rbind(data_seals_unique, unique_dives_seal)
}

summary_rm_dupli <- data_seals_unique %>% 
  group_by(id = REF) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  as.data.frame()

## Save compiled dive data
file_output =  "~/Desktop/WHOI/Data/behavioural_data/compiled_rm_dupli_dives"
saveRDS(data_seals_unique, file_output)

#==================================================================
# 2) UPDATE SEALS TABLE 
#==================================================================

seals = read.csv("~/Desktop/WHOI/Data/seals.csv")

no_dive_seals = setdiff(seals$REF, unique(data$REF)) # "ct164-309-BAT2-15" "ct164-185-BAT-20" "ct164-484-21" "ct7_10035_05"

seals$has_dives = 1
for (seal in no_dive_seals) {
  seals$has_dives[which(seals$REF == seal)] = 0
}

seals$n_dives_bf_filt = 0
for (seal in seals$REF) {
  nb_dives = length(which(data$REF == seal))
  seals$n_dives_bf_filt[seals$REF == seal] = nb_dives
}

seals$n_dives_af_rm_dupli = seals$n_dives_bf_filt
for (seal in seals$REF) {
  if (seals$has_dives[seals$REF == seal] == 1) {
    seals$n_dives_af_rm_dupli[seals$REF == seal] = summary_rm_dupli$n_dives[summary_rm_dupli$id == seal]
  }
}

write.table(seals, "~/Desktop/WHOI/Data/seals.csv", 
            sep = ",", row.names = F, col.names = T)

## End script
rm(list=ls())

