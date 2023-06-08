##
## Script name: stat_diag_data
##
## Purpose of script: calculate new var and compute statistical 
##                    analysis on non filtered SES tracks
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
## dbs = ??? behavioral statistics
##
## mean and sd of time interval between loc on non filtered tracks
## start/end date, trip duration on non filtered tracks
##
## speed mean and max done on filtered tracks (not done here)
## distance from colony, distance cumulee on filtered tracks (not done here)
#-----------------------------------------------
rm(list=ls())

## Working directory
setwd("~/Desktop/WHOI/Data/")

## Library

## Import data
data <- readRDS("compiled_diag_non_filtered_tracks")

## Stats for each track
tracks = unique(data$REF)

locPerDay_tot <- NULL #??
timeDiff_tot <- NULL # all time diff between loc across all indiv (in sec) 

dbs <- matrix(nrow = 0, ncol = 15) 
dbs <- as.data.frame(dbs)
colnames(dbs) = c("ref", "m_timebtwloc", "sd_timebtwloc", "duration_diag", "start", "end",
                  "nb_loc", "sd_loc", "loc0", "loc1", "loc2", "loc3", "loc_1", "loc_2", "loc_9")


for (index in 1:length(tracks)) {
  subset_data <- data[data$REF == tracks[index], ]
  
  dbsTmp <- matrix(nrow = 1, ncol = 15) 
  dbsTmp <- as.data.frame(dbsTmp)
  colnames(dbsTmp) = c("ref", "m_timebtwloc", "sd_timebtwloc", "duration_diag", "start", "end",
                    "nb_loc", "sd_loc", "loc0", "loc1", "loc2", "loc3", "loc_1", "loc_2", "loc_9")
  
  ## Statistics on time intervals between loc for each track
  deploy_time <- diff(subset_data$D_DATE) # in sec
  timeDiff_tot <- c(timeDiff_tot, deploy_time)
  dbsTmp$m_timebtwloc <- ((mean(deploy_time)) / 60) / 60 # mean in h
  dbsTmp$sd_timebtwloc <- ((sd(deploy_time)) / 60) / 60 # sd in h
  dbsTmp$ref = subset_data$REF[1]
  
  dbsTmp$start = subset_data$DATE[1] #debut des loc
  dbsTmp$end= subset_data$DATE[nrow(subset_data)]#fin des loc
  dbsTmp$duration_diag = difftime(dbsTmp$end, dbsTmp$start, units = "days")
  
  ## Statistics on location quality (ARGOS): proportion of locations by LQ on overall locations
  dbsTmp$loc0 = (length(which(subset_data$LQ == 0)) / nrow(subset_data)) * 100
  dbsTmp$loc1 = (length(which(subset_data$LQ == 1)) / nrow(subset_data)) * 100
  dbsTmp$loc2 = (length(which(subset_data$LQ == 2)) / nrow(subset_data)) * 100
  dbsTmp$loc3 = (length(which(subset_data$LQ == 3)) / nrow(subset_data)) * 100
  dbsTmp$loc_1 = (length(which(subset_data$LQ == -1)) / nrow(subset_data)) * 100
  dbsTmp$loc_2 = (length(which(subset_data$LQ == -2)) / nrow(subset_data)) * 100
  dbsTmp$loc_9 = (length(which(subset_data$LQ == -9)) / nrow(subset_data)) * 100
  
  ## Statistics on locations number
  dates = unique(subset_data$DATE)
  locPerDay <- data.frame()
  
  for (day in dates) {
    dataOneDay <- subset_data[which(subset_data$DATE == day), ]
    n_loc <- length(dataOneDay$LAT)
    locPerDay <- rbind(locPerDay, n_loc)
    locPerDay_tot <-
      c(locPerDay_tot, n_loc) # all number of locations from all indiv
  }
  dbsTmp$nb_loc = sapply(locPerDay, mean)
  dbsTmp$sd_loc = sapply(locPerDay, sd)
  
  dbs = rbind(dbs, dbsTmp)
}
dbs$start = as.POSIXct(dbs$start, origin = "1970-01-01", tz = "GMT")
dbs$end = as.POSIXct(dbs$end, origin = "1970-01-01", tz = "GMT")

# Statistics on all tracks 
timeDiff_tot <- as.data.frame(timeDiff_tot)
locPerDay_tot = as.data.frame(locPerDay_tot) # nb de locs par jour pour tous

{
  sink('summary_stats_non_filtered_tracks.txt')
  cat(nrow(timeDiff_tot), "locations")
  cat("\n\n")
  print(summary((timeDiff_tot / 60)/60))
  cat(paste("sd:" , (sapply(timeDiff_tot, mean) / 60) / 60))
  cat("\n#############################\n\n")
  print(summary(locPerDay_tot))
  cat(paste("sd:", sapply(locPerDay_tot, sd)))
  sink()
}

## Save dbs as R object or CSV
saveRDS(dbs, "stat_diag_non_filtered_tracks")
write.csv(dbs, file = "stat_diag_non_filtered_tracks.csv", row.names = T)
