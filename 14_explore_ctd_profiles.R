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
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/update_seals_table.R")
## ---------------------------

stations <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table")

seals <- read.csv("~/Desktop/WHOI/Data/output_data/seals.csv")
seals$REF <- gsub("_", "-", seals$REF)

seals %>%
  filter(!REF %in% stations$REF) %>%
  select(c(REF, has_dives, visit_polynya))

# Step0: number of CTD profiles per seal
step0 <- stations %>%
  group_by(REF) %>%
  summarise(n_ctd_step0 = n())
  
seals <- seals %>% left_join(step0, by = "REF")

# Step1: number of CTD profiles under northern boundary or latitude = NA
northern_boundary <- -60

step1 <- stations %>%
  filter(is.na(lat) | lat < northern_boundary) %>%
  group_by(REF) %>%
  summarise(n_ctd_step1 = n()) 

seals <- seals %>% left_join(step1, by = "REF")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/output_data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

#==================================================================
# 2) REESTIMATE CTD LOCATIONS WITH SSM
#==================================================================
rm(list=ls())

filter_on_ssm_accuracy = TRUE
threshold = 20 #km
north_boundary = -60
# TRUE => ctd stations closest in time from poorly accurate predicted locations are filtered with a upper threshold on standard error. 
# FALSE => all ctd locations are kept

stations <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table")
SSM = readRDS("~/Desktop/WHOI/Data/output_data/predictedTracks_ssm_ctd")
seals <- read.csv("~/Desktop/WHOI/Data/output_data/seals.csv")

df_interpol = NULL
seals = unique(SSM$id)

for (p in 1:length(seals)) {
  print(seals[p])
  sa <- stations %>%
    filter(REF == seals[p])
  ssm <- SSM %>%
    filter(id == seals[p])

  if (filter_on_ssm_accuracy) {
    qual_index <- as.data.frame(sa$id_ctd)
    qual_index$x.se = unlist(sapply(sa$time, function(x) ssm$x.se[which(abs(x - ssm$date) == min(abs(x - ssm$date)))[1]])) 
    qual_index$y.se = unlist(sapply(sa$time, function(x) ssm$y.se[which(abs(x - ssm$date) == min(abs(x - ssm$date)))[1]])) 
    
    if (nrow(qual_index) > 0) {
      rm_rows <- subset(qual_index, x.se > threshold | y.se > threshold)
      sa <- sa %>% filter(!(id_ctd %in% rm_rows$`sa$id_ctd`))
    }
  }
  
  interpLon <- approx(ssm$date, ssm$lon, xout = sa$time)
  sa$interpLon <- interpLon$y
  interpLat <- approx(ssm$date, ssm$lat, xout = sa$time)
  sa$interpLat <- interpLat$y
  
  df_interpol <- rbind(df_interpol, sa)
}

file_interp_loc = "~/Desktop/WHOI/Data/output_data/ctd_stations_table_interp"
saveRDS(df_interpol, file_interp_loc)

# Step2: number of CTD profiles after reinterpolation (interpLat < northern boundary)
northern_boundary <- -60

seals <- read.csv("~/Desktop/WHOI/Data/output_data/seals.csv")

step2 <- df_interpol %>%
  filter(is.na(interpLat) | interpLat < northern_boundary) %>%
  group_by(REF) %>%
  summarise(n_ctd_step2 = n()) 

seals <- seals %>% left_join(step2, by = "REF")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/output_data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

## End script
rm(list=ls())
