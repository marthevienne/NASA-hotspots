##
## Script name: compute_dives_table.R
##
## Purpose of script: Create dives table in database with :
##                    dives {REF, NUM, time, depth}
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-04
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

## Library
library(ggplot2)
library(scriptName)

## Get script name (used for source run)
filename <- current_filename()
filename <- sub(".*/", "", filename)

## Data input
seals = read.csv("~/Desktop/WHOI/Data/output_data/seals.csv")
dives = readRDS("dive_metrics_V2")

#==================================================================
# CREATE DIVES TABLE {REF, NUM, time, depth}
#==================================================================

nobs = 6 # number of observations for each dive (nb of time-depth pairs)

num_array = NULL
for (seal in seals$REF) {
  nb_dives = length(which(dives$REF == seal))
  num_dives = subset(dives, REF == seal, select = NUM)
  if (nb_dives > 0) {
    num_array = c(num_array, rep(num_dives$NUM, each = nobs)) #___NUM
  }
}

ref_array = subset(data.frame(lapply(seals, rep, seals$n_dives_SSM_filter * nobs)), select = REF) #_____REF

depth_obs = subset(dives, select = c(D0, D1, D2, D3, D4, Df))
time_obs = subset(dives, select = c(T0, T1, T2, T3, T4, Tf))

depth_array = as.vector(t(as.matrix(depth_obs))) #_____depth
time_array = as.vector(t(as.matrix(time_obs))) #_____time

dives_SES = as.data.frame(cbind(ref_array, num_array, time_array, depth_array))
colnames(dives_SES) = c("REF", "NUM", "time", "depth")
dives_SES$depth = as.numeric(dives_SES$depth)
dives_SES$time = as.numeric(dives_SES$time)

## Save dives table as R object
file_output = "~/Desktop/WHOI/Data/output_data/dives_profiles"
saveRDS(dives_SES, file_output)
#write.csv(dives_SES, "dives_table.csv", row.names = F)
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "Dive table saved : ", file_output))
}

## End script
rm(list=ls())

