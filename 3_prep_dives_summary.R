##
## Script name: prep_dives_summary
##
## Purpose of script: 1) Assign a number to each dive
##                    2) Filtering workflow -> update seals table with # dives remaining after each step
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
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
## ---------------------------
## Library
library(dplyr)
library(readxl)
library(ggplot2)
library(scriptName)
## ---------------------------
## Paths
path_fig = 
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/update_seals_table.R")
source("~/Desktop/WHOI/Codes/useful_functions/my_SSM_filter.R")
## ---------------------------

## Data input
dives <- readRDS("~/Desktop/WHOI/Data/behavioural_data/compiled_rm_dupli_dives")
seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")

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
file_dives_num = "~/Desktop/WHOI/Data/behavioural_data/non_filtered_numbered_dives"
saveRDS(dives_num, file_dives_num)

rm(dives_num)

#==================================================================
# 2) FILTERING WORKFLOW
#==================================================================

dives_num <- readRDS("~/Desktop/WHOI/Data/behavioural_data/non_filtered_numbered_dives")

#Step0: figures before filtering dives
indiv = unique(dives_num$REF)

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
file_summary = "~/Desktop/WHOI/Data/summary_outputs/summary_filt_dives_output.txt"
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

fig_name <- "~/Dropbox/data/outputs_Marthe_2023/MAX_DEP_VS_DIVE_DUR_scatter_plot_bf_filt.png"
{
  png(
    fig_name,
    width = 1000,
    height = 1000,
    res = 150
  )
  print(ggplot(dives_num, aes(x = DIVE_DUR, y = MAX_DEP)) +
          geom_point())
  dev.off()
}

#----------------
#Step1: select only dives_num south of a boundary and lat == NA
north_boundary = -60
dives_num <- dives_num[dives_num$LAT < north_boundary | is.na(dives_num$LAT), ]
seals <- update_seals_table(dives_num, seals, "n_dives_stp1")

#Step 2
dives_num = dives_num[!is.na(dives_num$DE_DATE),]
seals <- update_seals_table(dives_num, seals, "n_dives_stp2")

#Step 3
dives_num <- dives_num[dives_num$DIVE_DUR != 0,]
seals <- update_seals_table(dives_num, seals, "n_dives_stp3")

#Step 4
dives_num = dives_num[dives_num$T1 != 0,]
seals <- update_seals_table(dives_num, seals, "n_dives_stp4")

#Step 5
dives_num <-
  dives_num[dives_num$T1 < dives_num$T2 &
              dives_num$T2 < dives_num$T3 & dives_num$T3 < dives_num$T4,]
seals <- update_seals_table(dives_num, seals, "n_dives_stp5")

#Step 6
dives_num <- dives_num[dives_num$MAX_DEP < 2000, ]
seals <- update_seals_table(dives_num, seals, "n_dives_stp6")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

## Save filtered dives (tracks non corrected)
file_filt_dives_num = "~/Desktop/WHOI/Data/behavioural_data/filtered_numbered_dives"
saveRDS(dives_num, file_filt_dives_num)

#==================================================================
# PRELIMINARY PLOTS ON DIVE dives_num (after filtering)
#==================================================================

#dives_num = readRDS(paste0(path_input, "filtered_numbered_dives"))

{
  pdf(
    "~/Dropbox/data/outputs_Marthe_2023/histograms_dives_all_dives_num_SES_after_filt.pdf",
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
    "~/Dropbox/data/outputs_Marthe_2023/MAX_DEP_VS_DIVE_DUR_scatter_plot_after_filt.png",
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
# 3) COMPUTE NEW VARIABLES: BOTT_TIME, SPEED_DESC, SPEED_ASC, D0, Df, T0, T1, T2, T3, T4, Tf 
#     => T in sec since 01/01/1970
#     /!\ locations are not estimated at that point with BSSM output
#=====================================================================================================
# rm(list=ls())

seals = read.csv("~/Desktop/WHOI/Data/seals.csv")
dives_num = readRDS("~/Desktop/WHOI/Data/behavioural_data/filtered_numbered_dives")
dives_num = dives_num[dives_num$MAX_DEP >= 5,]

seals <- update_seals_table(dives_num, seals, "n_dives_below_5m")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

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

T1_BP <- NULL
Tf_BP <- NULL
D0_BP <- NULL
Df_BP <- NULL

T1_BP_f <- NULL
Tf_BP_f <- NULL
D0_BP_f <- NULL
Df_BP_f <- NULL

for (index in 1:nrow(df_dives)) {
  dive = df_dives[index, ]
  ref_depth <- c(dive$D0, dive$D1, dive$D2, dive$D3, dive$D4, dive$Df)
  tim <- c(dive$T0, dive$T1, dive$T2, dive$T3, dive$T4, dive$Tf)
  seq_t <- seq(dive$T0, dive$Tf, 2)
  interp <- approx(tim, ref_depth, xout = seq_t, method = "linear")
  bottdep <- 0.8 * dive$MAX_DEP # ?????? HOW IS IT DEFINED?
  t <- interp$x[interp$y >= bottdep]
  
  if (length(t) != 0) {
    T1_BP <- c(T1_BP, t[1])
    Tf_BP <- c(Tf_BP, t[length(t)])
    D0_BP <- c(D0_BP, interp$y[interp$x == t[1]])
    Df_BP <- c(Df_BP, interp$y[interp$x == t[length(t)]])
  } else {
    T1_BP <- c(T1_BP, NA)
    Tf_BP <- c(Tf_BP, NA)
    D0_BP <- c(D0_BP, NA)
    Df_BP <- c(Df_BP, NA)
  }
}

df_dives$BOTT_TIME <- Tf_BP - T1_BP
df_dives$SPEED_DESC <- (D0_BP - df_dives$D0) / (T1_BP - df_dives$T0)
df_dives$SPEED_ASC <- (df_dives$Df - Df_BP) / (df_dives$Tf - Tf_BP)

df_dives = subset(df_dives, select = c(REF, NUM, SOL_ANGLE, MAX_DEP, DIVE_DUR, BOTT_TIME, SURF_DUR, SPEED_DESC, SPEED_ASC, 
                                       DE_DATE, D0, D1, D2, D3, D4, Df, T0, T1, T2, T3, T4, Tf, START_LON, START_LAT))


## Save updated dive summary
file_metrics_dives = "~/Desktop/WHOI/Data/behavioural_data/dive_metrics_bottime_speed"
saveRDS(df_dives, file_metrics_dives)

rm(list = c(dives_num, df_dives))


#==================================================================
# 4) ESTIMATE DIVES LOCATIONS WITH BSSM
#==================================================================
rm(list=ls())

filter_dive_on_bssm_accuracy = TRUE
threshold = 10 #km
north_boundary = -60
# TRUE => dives closest in time from poorly accurate predicted locations are filtered with a upper threshold on standard error. 
# FALSE => all dive locations are kept

dives <- readRDS("~/Desktop/WHOI/Data/behavioural_data/dive_metrics_bottime_speed_1")
dives$DE_DATE <- as.POSIXct(dives$DE_DATE, origin = "1970-01-01", tz = "GMT")
str(dives)

BSSM = readRDS("~/Desktop/WHOI/Data/bssm/predictedTracks_ssm")

seals <- read.csv("~/Desktop/WHOI/Data/seals.csv")

df_dives_interpol = NULL
seals = unique(BSSM$id)

for (p in 1:length(seals)) {
  print(seals[p])
  sa <- dives[dives$REF == seals[p],]
  bssm <- BSSM[BSSM$id == seals[p],]
  
  if (filter_dive_on_bssm_accuracy) {
    dive_qual_index <- as.data.frame(sa$NUM)
    dive_qual_index$x.se = unlist(sapply(sa$DE_DATE, function(x) bssm$x.se[which(abs(x - bssm$date) == min(abs(x - bssm$date)))[1]])) 
    dive_qual_index$y.se = unlist(sapply(sa$DE_DATE, function(x) bssm$y.se[which(abs(x - bssm$date) == min(abs(x - bssm$date)))[1]])) 
    
    if (nrow(dive_qual_index) > 0) {
      rm_dives <- subset(dive_qual_index, x.se > threshold | y.se > threshold)
      sa <- sa %>% filter(!(NUM %in% rm_dives$`sa$NUM`))
    }
  }
  
  interpLon <- approx(bssm$date, bssm$lon, xout = sa$DE_DATE)
  sa$interpLon <- interpLon$y
  interpLat <- approx(bssm$date, bssm$lat, xout = sa$DE_DATE)
  sa$interpLat <- interpLat$y
  
  df_dives_interpol <- rbind(df_dives_interpol, sa)
}

str(df_dives_interpol)

# Remove dives with NA locations -> these are the dives that do not occur in the track timeline
#                                   hence the interpolation returns NA

df_dives_interpol <- df_dives_interpol %>% filter(!(is.na(interpLon) | is.na(interpLat)))
df_dives_interpol <- df_dives_interpol %>% filter(interpLat < north_boundary)
range(df_dives_interpol$interpLat, na.rm = T)

seals <- update_seals_table(df_dives_interpol, seals, "n_dives_SSM_filter")

## Save updated seals table ======================================
file_output_seals =  "~/Desktop/WHOI/Data/seals.csv"
write.table(seals, file_output_seals, sep = ",", row.names = F, col.names = T)
##================================================================

## Save dives with interpolated locations in R object
file_metrics_dives_interp_loc = "~/Desktop/WHOI/Data/behavioural_data/dive_metrics_bottime_speed_interp_2"
saveRDS(df_dives_interpol, file_metrics_dives_interp_loc)

## End script 
rm(list=ls())

