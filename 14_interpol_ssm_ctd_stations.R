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
library(dplyr)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

stations <- readRDS("~/Desktop/WHOI/Data/ctd_data/ctd_stations_table")

seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")
seals <- seals %>%
  select(!contains("ctd"))
seals$REF <- gsub("_", "-", seals$REF)

seals %>%
  filter(!REF %in% stations$REF) %>%
  filter(visit_polynya == "yes") %>%
  select(c(REF, has_dives, visit_polynya)) # either seals with no ctd files or with NA ctd data

length(unique(stations$REF))

# Step0: number of CTD profiles per seal
step0 <- stations %>%
  group_by(REF) %>%
  summarise(n_ctd_step0 = n())
  
seals <- seals %>% left_join(step0, by = "REF")

# Step1: number of CTD profiles under northern boundary or latitude = NA
northern_boundary <- -60

stations <- stations %>%
  filter(is.na(lat) | lat < northern_boundary)

file_loc = "~/Desktop/WHOI/Data/ctd_stations_table_north_bound"
saveRDS(stations, file_loc)

step1 <- stations %>%
  group_by(REF) %>%
  summarise(n_ctd_step1 = n()) 

seals <- seals %>% left_join(step1, by = "REF")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

#==================================================================
# 2) REESTIMATE CTD LOCATIONS WITH SSM
#==================================================================
rm(list=ls())

threshold = 5 #km
north_boundary = -60
filter_on_ssm_accuracy = FALSE
# TRUE => ctd stations closest in time from poorly accurate predicted locations are filtered with a upper threshold on standard error. 
# FALSE => all ctd locations are kept

stations <- readRDS("~/Desktop/WHOI/Data/ctd_data/ctd_stations_table_north_bound")
SSM = readRDS("~/Desktop/WHOI/Data/bssm/predictedTracks_ssm")
seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")

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
      rm_rows <- subset(qual_index, sqrt(x.se^2 + y.se^2) > threshold)
      sa <- sa %>% filter(!(id_ctd %in% rm_rows$`sa$id_ctd`))
    }
  }
  
  interpLon <- approx(ssm$date, ssm$lon, xout = sa$time)
  sa$interpLon <- interpLon$y
  interpLat <- approx(ssm$date, ssm$lat, xout = sa$time)
  sa$interpLat <- interpLat$y
  
  df_interpol <- rbind(df_interpol, sa)
}

nrow(df_interpol) / nrow(stations) * 100 #___ % kept stations

file_interp_loc = "~/Desktop/WHOI/Data/ctd_data/ctd_stations_table_interp"
saveRDS(df_interpol, file_interp_loc)

# Step2: number of CTD profiles after estimation of location (interpLat != NA)
seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")

df_interpol <- df_interpol %>%
  filter(!is.na(interpLat))

step2 <- df_interpol %>%
  group_by(REF) %>%
  summarise(n_ctd_step2 = n()) 

seals <- seals %>% left_join(step2, by = "REF")

# Step 3: number of CTD stations south of north boundary after estimation of location
north_boundary <- -60

df_interpol <- df_interpol %>%
  filter(interpLat < north_boundary)

step3 <- df_interpol %>%
  group_by(REF) %>%
  summarise(n_ctd_step3 = n())

seals <- seals %>% left_join(step3, by = "REF")

file_interp_loc = "~/Desktop/WHOI/Data/ctd_data/ctd_stations_table_north_bound_interp"
saveRDS(df_interpol, file_interp_loc)

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

#==================================================================
# 3) SUMMARY FILTERING PROCESS
#==================================================================
rm(list=ls())

seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")

summary_ctd <- seals %>%
  select(contains("ctd"))

tot <- colSums(summary_ctd, na.rm = T)
tot / tot[1] * 100

description <- paste0("unit: % \n",
                      "Step 0: number of CTD profiles per seal \n",
                      "Step 1: number of CTD profiles under northern boundary or latitude = NA \n",
                      "Step 2: number of CTD profiles after estimation of location (interpLat != NA) \n",
                      "Step 3: number of CTD stations south of north boundary after estimation of location \n")
#-----Save output
sink("~/Desktop/WHOI/Data/summary_outputs/filtering_ctd_stations.txt")
cat(paste("Last modified :", Sys.Date()))
cat("\n\n")
cat(description)
cat("\n")
print(tot / tot[1] * 100)
cat("\n")
sink()
#----------------

## End script
rm(list=ls())
