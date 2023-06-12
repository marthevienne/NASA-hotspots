##
## Script name: calculate_mean_depth_long_seg.R
##
## Purpose of script:
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
## Notes:
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

## Mean depth of longer segment

## Dive metrics
dives <- readRDS("dive_metrics_V3")

## Dive profiles
profiles <- readRDS("dives_profiles")

seg <- profiles %>%
  group_by(REF, NUM) %>%
  reframe(Tdiff = c(diff(time), NA), mean_depth = depth + c(diff(depth)/2, NA)) %>%
  na.omit()

time_long_seg <- seg %>%
  group_by(REF, NUM) %>%
  reframe(time_longer_seg = max(Tdiff))

long_seg <- time_long_seg %>% 
  left_join(seg, by = c('REF','NUM', "time_longer_seg" = "Tdiff"))

# Dives that have more than one longest segments (e.g. same segment duration)
long_seg_sum <- long_seg %>%
  group_by(REF, NUM) %>%
  summarise(n = n()) %>%
  filter(n > 1)

nrow(long_seg_sum)/nrow(long_seg) * 100

## Assign mean depth of longer segment to dive metrics table
# toto <- dives %>% full_join(long_seg, by = c('REF','NUM'))
# toto <- toto %>%
#   rename(mean_depth_long_seg = mean_depth)

# summary(dives$mean_depth_long_seg)
# which(is.na(dives$mean_depth_long_seg))
# 
# dive <- dives[5526,  ]

saveRDS(long_seg, "longer_segment_dives")

## End script
rm(list=ls())

