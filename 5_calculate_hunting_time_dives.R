##
## Script name: calculate_hunting_time_dives.R
##
## Purpose of script: 1) Compute hunting time for each dive on dive profiles table (REF, NUM, time, depth)
##                    2) Assign hunting time to dive metrics table by REF, NUM 
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-08
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes: Segments with speed <= 0.4 m/s are defined as hunting segments. 
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
setwd("~/Desktop/WHOI/Data/output_data/")
## ---------------------------
## Library
library(dplyr)
## ---------------------------

## Dive data
dives <- readRDS("dive_metrics_V2")
str(dives)

## Dive profiles
profiles <- readRDS("dives_profiles")

## Calculate speed for each segment
profiles_speed <- profiles %>%
  group_by(REF, NUM) %>%
  reframe(speed_seg = abs(diff(depth))/diff(time),
          Tdiff = diff(time)) %>%
  filter(speed_seg <= 0.4) #___ref article

## Calculate hunting time => sum of time segment with speed <= 0.4 m/s
hunting_time <- profiles_speed %>%
  group_by(REF, NUM) %>%
  reframe(hunting_time = sum(Tdiff))

## Assign hunting time to dive metrics table
dives <- dives %>% left_join(hunting_time, by = c('REF','NUM'))
dives$hunting_time[is.na(dives$hunting_time)] = 0 #___put NA hunting time to 0

summary(dives$hunting_time)

## Save dives
saveRDS(dives, "dive_metrics_V3")

## End script
rm(list=ls())
