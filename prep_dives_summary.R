##
## Script name: prep_dives_summary
##
## Purpose of script: 1) Assign a number to each dive
##                    2) Filtering workflow
##                    3) Compute new variables
##                    4) Estimate dives locations with BSSM locations
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
##
## ---------------------------
rm(list = ls())

source("~/Desktop/WHOI/Codes/my_SSM_filter.R")

## Path input data
path_input = "~/Desktop/WHOI/Data/output_data/"

## Path output data
path_output = "~/Desktop/WHOI/Data/output_data/"

## Path to save output figures
path_fig = "~/Dropbox/data/outputs_Marthe_2023/"

## Path output summary 
file_summary = "~/Desktop/WHOI/Data/summary_outputs/summary_filt_dives_output.txt"

## Library
library(dplyr)
library(readxl)
library(ggplot2)
library(scriptName)

## Get script name (used for source run)
filename <- current_filename()
filename <- sub(".*/", "", filename)

## Data input
dives <- readRDS(paste0(path_input, "compiled_rm_dupli_dives"))

#==================================================================
# 1) ASSIGN A NUMBER TO EACH DIVE (NOT UNIQUE) 
#     => identify dive by REF, NUM
#==================================================================

indiv = unique(dives$REF)
n_var = ncol(dives) + 1
dives_num = matrix(nrow = 0, ncol = n_var)
dives_num <- as.data.frame(dives_num)
colnames(dives_num) = colnames(dives)

for (i in 1:length(indiv)) {
  print(i)
  subset_dives <- dives[dives$REF == indiv[i],]
  subset_dives <- subset_dives[with(subset_dives, order(subset_dives$DE_DATE)), ]
  subset_dives$NUM <- rep(1:nrow(subset_dives), 1)
  dives_num <- rbind(dives_num, subset_dives)
}
rm(subset_dives)
rm(dives)

## Save numbered dive dives_num
file_dives_num = paste0(path_output, "non_filtered_numbered_dives")
saveRDS(dives_num, file_dives_num)
#write.csv(dives_num, file =  file_dives_num, row.names = FALSE)
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "Numbered dive data saved : ", file_dives_num))
}

rm(dives_num)

#==================================================================
# 2) FILTERING WORKFLOW
#==================================================================

dives_num <- readRDS(paste0(path_input, "non_filtered_numbered_dives"))

nb_dives_array = NULL
nb_indiv_array = NULL

#Step0: figures before filtering dives
indiv = unique(dives_num$REF)
nb_dives_array <- c(nb_dives_array, "step0" = nrow(dives_num))
nb_indiv_array <- c(nb_indiv_array, "step0" = length(indiv))

## Percentage of dives with T1 > T2 or T2 > T3
tot = which(dives_num$T2 <= dives_num$T1 |
              dives_num$T3 <= dives_num$T2 | dives_num$T4 <= dives_num$T3)
p1 = length(tot) / length(dives_num$T2) * 100
rm(tot)

## Percentage of dives with dive duration = 0
p2 = length(which(dives_num$DIVE_DUR == 0)) / length(dives_num$DIVE_DUR) * 100

## Percentage of dives with end date = NA
p3 = length(which(is.na(dives_num$DE_DATE))) / length(dives_num$DE_DATE) * 100

## Percentage of dives with relative duration T1 = 0
p4 = length(which(dives_num$T1 == 0)) / length(dives_num$T1) * 100

#-----Save summary output
{
  sink(file_summary)
  cat("=============================\n")
  cat("Raw dive dives_num\n")
  cat("=============================\n\n")
  cat(paste("Last modified :", Sys.Date(), "\n"))
  cat(sprintf("Number of individuals: %s \n", length(indiv)))
  cat(sprintf("Number of dives: %s \n", nrow(dives_num)))
  cat("\n")
  cat(sprintf("Percentage of dives with abnormal time percentage: %.2f %% \n", p1))
  cat(sprintf("Percentage of dives with dive duration = 0: %.2f %% \n", p2))
  cat(sprintf("Percentage of dives with end date = NA: %.2f %% \n", p3))
  cat(sprintf("Percentage of dives with relative duration T1 = 0: %.2f %% \n", p4))
  cat("\n")
  sink()
}
rm(p1)
rm(p2)
rm(p3)
rm(p4)

{
  png(
    paste0(path_fig, "MAX_DEP_VS_DIVE_DUR_scatter_plot_bf_filt.png"),
    width = 1000,
    height = 1000,
    res = 150
  )
  print(ggplot(dives_num, aes(x = DIVE_DUR, y = MAX_DEP)) +
          geom_point())
  dev.off()
}
#----------------

#Step1: select only dives_num south of a boundary
{
  north_boundary = -58.5
  dives_num = dives_num[dives_num$LAT < north_boundary,]
  indiv = unique(dives_num$REF) # update indiv
  nb_dives_array <- c(nb_dives_array, "step1" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step1" = length(indiv))
  
  ## Percentage of dives with T1 > T2 or T2 > T3
  tot = which(dives_num$T2 <= dives_num$T1 |
                dives_num$T3 <= dives_num$T2 | dives_num$T4 <= dives_num$T3)
  p1 = length(tot) / length(dives_num$T2) * 100
  rm(tot)
  
  ## Percentage of dives with dive duration = 0
  p2 = length(which(dives_num$DIVE_DUR == 0)) / length(dives_num$DIVE_DUR) * 100
  
  ## Percentage of dives with end date = NA
  p3 = length(which(is.na(dives_num$DE_DATE))) / length(dives_num$DE_DATE) * 100
  
  ## Percentage of dives with relative duration T1 = 0
  p4 = length(which(dives_num$T1 == 0)) / length(dives_num$T1) * 100
  
  ## Save output
  sink(file_summary, append = TRUE)
  cat("=============================\n")
  cat("Filtering dives \n")
  cat("=============================\n\n")
  cat(sprintf("Step 1: Dives south of %s°N \n", north_boundary))
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step1"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step1"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step1"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  cat(sprintf("Percentage of dives with abnormal time percentage: %.2f %% \n", p1))
  cat(sprintf("Percentage of dives with dive duration = 0: %.2f %% \n", p2))
  cat(sprintf("Percentage of dives with end date = NA: %.2f %% \n", p3))
  cat(sprintf("Percentage of dives with relative duration T1 = 0: %.2f %% \n", p4))
  cat("\n")
  sink()
  rm(p1)
  rm(p2)
  rm(p3)
  rm(p4)
  
}

#Step 2
{
  dives_num = dives_num[!is.na(dives_num$DE_DATE),]
  indiv = unique(dives_num$REF) #update indiv
  nb_dives_array <- c(nb_dives_array, "step2" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step2" = length(indiv))
}

#Step 3
{
  dives_num <- dives_num[dives_num$DIVE_DUR != 0,]
  indiv = unique(dives_num$REF) #update indiv
  nb_dives_array <- c(nb_dives_array, "step3" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step3" = length(indiv))
}

#Step 4
{
  dives_num = dives_num[dives_num$T1 != 0,]
  indiv = unique(dives_num$REF) #update indiv
  nb_dives_array <- c(nb_dives_array, "step4" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step4" = length(indiv))
}

#Step 5
{
  dives_num <-
    dives_num[dives_num$T1 < dives_num$T2 &
               dives_num$T2 < dives_num$T3 & dives_num$T3 < dives_num$T4,]
  indiv = unique(dives_num$REF) #update indiv
  nb_dives_array <- c(nb_dives_array, "step5" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step5" = length(indiv))
}

#Step 6
{
  dives_num <- dives_num[dives_num$MAX_DEP < 2000, ]
  indiv = unique(dives_num$REF) #update indiv
  nb_dives_array <- c(nb_dives_array, "step6" = nrow(dives_num))
  nb_indiv_array <- c(nb_indiv_array, "step6" = length(indiv))
}


## Save filtered dives (tracks non corrected)
file_filt_dives_num = paste0(path_output, "filtered_numbered_dives")
saveRDS(dives_num, file_filt_dives_num)
#write.csv(dives_num, file = file_filt_dives_num, row.names = FALSE)
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "Filtered numbered dive data saved : ", file_filt_dives_num))
}

#-----Save output
{
  sink(file_summary, append = TRUE)
  cat("Step 2: remove dives with end date dives_num == NA \n")
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step2"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step2"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step2"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  cat("Step 3: remove dives with dive duration == 0 \n")
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step3"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step3"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step3"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  cat("Step 4: remove dives with relative duration at T1 == 0 \n")
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step4"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step4"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step4"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  cat("Step 5: remove dives where T1 >= T2, T2 >= T3 or T3 >= T4 \n")
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step5"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step5"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step5"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  cat("Step 6: remove dives where max depth >= 2000 \n")
  cat("-----------------------------\n")
  cat(sprintf("Number of individuals: %s \n", nb_indiv_array["step6"]))
  cat(sprintf("Number of dives: %s \n", nb_dives_array["step6"]))
  cat(
    sprintf(
      "Percentage of dives remaining: %.2f %%\n",
      nb_dives_array["step6"] / nb_dives_array["step0"] * 100
    )
  )
  cat("\n")
  sink()
}
#----------------

if (length(nchar(filename)) != 0) {
  print(paste0(filename, " : analysis summary saved in ", file_summary))
}


#==================================================================
# PRELIMINARY PLOTS ON DIVE dives_num (after filtering)
#==================================================================

#dives_num = readRDS(paste0(path_input, "filtered_numbered_dives"))

{
  pdf(
    paste0(path_fig, "histograms_dives_all_dives_num_SES_after_filt.pdf"),
    height = 5,
    width = 6
  )
  print(
    hist(
      dives_num$DIVE_DUR / 60,
      breaks = 30,
      xlab = "Dive duration (in min)",
      main = "Dive duration histogram"
    )
  )
  print(
    hist(
      dives_num$MAX_DEP,
      breaks = 30,
      xlab = "Maximum depth (in m)",
      main = "Maximum depth histogram"
    )
  )
  dev.off()
}

{
  png(
    paste0(path_fig, "MAX_DEP_VS_DIVE_DUR_scatter_plot_after_filt.png"),
    width = 1000,
    height = 1000,
    res = 150
  )
  print(ggplot(dives_num, aes(x = DIVE_DUR, y = MAX_DEP)) +
          geom_point())
  dev.off()
}

rm(dives_num)

#=====================================================================================================
# 3) COMPUTE NEW VARIABLES: BOTT_TIME, SURF_DUR, SPEED_DESC, SPEED_ASC, D0, Df, T0, T1, T2, T3, T4, Tf 
#     => T in sec since 01/01/1970
#     /!\ locations are not estimated at that point with BSSM output
#=====================================================================================================

dives_num = readRDS(paste0(path_input, "filtered_numbered_dives"))
dives_num = dives_num[dives_num$MAX_DEP >= 5,]

df_dives = subset(dives_num, 
                select = c(REF, NUM, MAX_DEP, DIVE_DUR, SURF_DUR, DE_DATE, D1, D2, D3, D4, START_LON, START_LAT))
df_dives$SOL_ANGLE = NA
df_dives$BOTT_TIME = NA
df_dives$SPEED_DESC = NA
df_dives$SPEED_ASC = NA
df_dives$D0 = 5
df_dives$Df = 5

start_time = dives_num$DE_DATE - dives_num$DIVE_DUR
T1_time = dives_num$T1 * dives_num$DIVE_DUR / 100
T2_time = dives_num$T2 * dives_num$DIVE_DUR / 100
T3_time = dives_num$T3 * dives_num$DIVE_DUR / 100
T4_time = dives_num$T4 * dives_num$DIVE_DUR / 100
Tf_time = dives_num$DIVE_DUR

df_dives$T0 = as.numeric(ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "gmt") + as.numeric(start_time))
df_dives$T1 = as.numeric(start_time + T1_time)
df_dives$T2 = as.numeric(start_time + T2_time)
df_dives$T3 = as.numeric(start_time + T3_time)
df_dives$T4 = as.numeric(start_time + T4_time)
df_dives$Tf = as.numeric(start_time + Tf_time)


#####OPTIMISER !
for (index in 1:nrow(df_dives)) {
  if (index %% 10000 == 0) {
    print(index)
  }
  dive = df_dives[index, ]
  ref_depth <- c(dive$D0, dive$D1, dive$D2, dive$D3, dive$D4, dive$Df)
  tim <- c(dive$T0, dive$T1, dive$T2, dive$T3, dive$T4, dive$Tf)
  seq_t <- seq(dive$T0, dive$Tf, 2)
  interp <- approx(tim, ref_depth, xout = seq_t, method = "linear")
  bottdep <- 0.8 * dive$MAX_DEP # ?????? HOW IS IT DEFINED?
  t <- interp$x[interp$y >= bottdep] # ????
  
  if (length(t) == 0) {
    df_dives$BOTT_TIME[index] = NA
    df_dives$SPEED_DESC[index] = NA
    df_dives$SPEED_ASC[index] = NA
  } else {
    df_dives$BOTT_TIME[index] <- t[length(t)] - t[1]
    diff_t1 <- as.numeric(t[1] - dive$T0) # time between the start of the dive and the bottom portion of the dive 
    diff_d1 <- (interp$y[interp$x == t[1]]) - (dive$D0) # distance between the start of the dive and the bottom portion of the dive 
    diff_t2 <- as.numeric(dive$Tf - t[length(t)]) # time between the end of the dive and the bottom portion of the dive 
    diff_d2 <- -(dive$Df - (interp$y[interp$x == t[length(t)]])) # distance between the end of the dive and the bottom portion of the dive 
    
    df_dives$SPEED_DESC[index] = diff_d1 / diff_t1 # m/s
    df_dives$SPEED_ASC[index] = diff_d2 / diff_t2 # m/s
  }
}

df_dives = subset(df_dives, select = c(REF, NUM, SOL_ANGLE, MAX_DEP, DIVE_DUR, BOTT_TIME, SURF_DUR, SPEED_DESC, SPEED_ASC, 
                                       DE_DATE, D0, D1, D2, D3, D4, Df, T0, T1, T2, T3, T4, Tf, START_LON, START_LAT))


## Save updated dive summary with locations under north boundary and maximum depth > 5m and new variables
file_metrics_dives = paste0(path_output, "dive_metrics_V1")
saveRDS(df_dives, file_metrics_dives)
#write.csv(df_dives, file = file_metrics_dives, row.names = FALSE)
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "Metrics dives table saved : ", file_metrics_dives))
}

rm(list = c(dives_num, df_dives))

#==================================================================
# 4) ESTIMATE DIVES LOCATIONS WITH BSSM
#==================================================================

filter_predicted_SSM = FALSE
threshold = 10
# TRUE => predicted locations are filtered with a upper threshold on standard error. For locations with SE > threshold, raw locations are kept instead. 
# FALSE => all predicted locations are used for dive locations interpolation

dives <- readRDS(paste0(path_input, "dive_metrics_V1"))
dives$DE_DATE <- as.POSIXct(dives$DE_DATE, origin = "1970-01-01", tz = "GMT")
str(dives)

raw_tracks = readRDS(paste0(path_input, "tracks_for_bssm"))
BSSM = readRDS(paste0(path_input, "predictedTracks_ssm"))
if (filter_predicted_SSM) {
  BSSM = my_SSM_filter(raw_tracks, BSSM, threshold = threshold)
  # str(BSSM)
}

df_dives_interpol = NULL
seals = unique(BSSM$id)

for (p in 1:length(seals)) {
  pdf(paste0(path_fig, '/interpol_dives_SES/', sprintf("%s_intrpl_track.pdf", seals[p])), 
      height = 6, 
      width = 8)
  print(p)
  sa <- dives[dives$REF == seals[p],]
  bssm <- BSSM[BSSM$id == seals[p],]
  interpLon <- approx(bssm$date, bssm$lon, xout = sa$DE_DATE)
  sa$lon <- interpLon$y
  interpLat <- approx(bssm$date, bssm$lat, xout = sa$DE_DATE)
  sa$lat <- interpLat$y
  title = paste0(seals[p])
  if (min(interpLat$y) != Inf | max(interpLat$y) != -Inf) {
    ylim = range(interpLat$y)
  } else {
    ylim = range(bssm$lat)
  }
  print(plot(bssm$date, bssm$lat, ylim = ylim,
       xlab = "Time", ylab = "latitude",
       main = title, pch = 4))
  print(points(sa$DE_DATE, interpLat$y, col = "lightblue", pch = 16))
  print(plot(bssm$date, bssm$lon,
             xlab = "Time", ylab = "longitude",
             main = title))
  print(points(sa$DE_DATE, interpLon$y, col = "lightblue", pch = 16))
  dev.off()
  df_dives_interpol <- rbind(df_dives_interpol, sa)
}

str(df_dives_interpol)

## Save dives with interpolated locations in R object and .txt
file_metrics_dives_interp_loc = paste0(path_output, "interp_dive_metrics_V2")
saveRDS(df_dives_interpol, file_metrics_dives_interp_loc)
# write.table(df_dives_interpol,
#             file = file_metrics_dives_interp_loc,
#             sep = ",",
#             row.names = F)
if (length(nchar(filename)) != 0) {
  print(paste0(filename, " | ", "Dive metrics table saved : ", file_metrics_dives_interp_loc))
}

## End script 
rm(list=ls())

