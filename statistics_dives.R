## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-19
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
setwd("~/Desktop/WHOI/Data/")
## ---------------------------
## Library
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Selected polynyas for analysis
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")
selection

## Import dives data
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol %in% selection) %>%
  mutate(month = month(DE_DATE)) %>%
  filter(month <= 8) #___summer-winter

## Import polynyas info
polynya_info <- read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
names_pol <- paste0(polynya_info$ID, " | ", polynya_info$Name)
names(names_pol) = polynya_info$ID

#==================================================================
# 1) STATISTICS DIVES
#==================================================================

## Statistics
pel_dives <- dives %>%
  group_by(pol) %>%
  count(dive_mode) %>%
  mutate(ndives = sum(n)) %>%
  mutate(perc = n/ndives*100)

## Stacked barplot fraction dive mode
pal_mode <- c("pelagic" = "#348FA7FF", 
              "benthic" = "#FEBA80FF")

bp_dive_mode <- ggplot(pel_dives) +
  geom_bar(aes(x = factor(pol), y = perc, fill = dive_mode),
           position="fill", stat="identity") +
  scale_fill_manual("Dive mode", values = pal_mode, na.value = "#E5E5E5") +
  theme_minimal() +
  scale_y_continuous(n.breaks = 10) +
  ylab("Fraction") +
  xlab("") +
  scale_x_discrete(labels = names_pol) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/barplot_dive_mode.png",
    height = 10,
    width = 10, units = "cm", res = 300)
print(bp_dive_mode)
dev.off()


shelf_dives <- dives %>%
  group_by(pol) %>%
  count(zone) %>%
  mutate(ndives = sum(n)) %>%
  mutate(shelf = n/ndives*100) %>%
  filter(zone == "shelf") %>%
  ungroup() %>%
  dplyr::select(c(pol, shelf)) %>%
  mutate(slope = 100 - shelf) 

pol_id = shelf_dives$pol

shelf_dives <- shelf_dives %>%
  gather(pol) %>%
  rename(zone = pol) %>%
  mutate(pol = rep(pol_id, 2))

bp_zone <- ggplot(shelf_dives) +
  geom_bar(aes(x = factor(pol), y = value, fill = zone),
           position="fill", stat="identity") +
  scale_fill_manual("Zone", values = c("#80CAAC", "#FDDC9EFF")) +
  theme_minimal() +
  scale_y_continuous(n.breaks = 10) +
  ylab("Fraction") +
  xlab("") +
  scale_x_discrete(labels = names_pol) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/barplot_zone.png",
    height = 10,
    width = 10, units = "cm", res = 300)
print(bp_zone)
dev.off()


pel_dives <- pel_dives %>%
  dplyr::select(c(pol, dive_mode, perc)) %>%
  ungroup() %>%
  spread(key = dive_mode, value = perc) %>%
  rename("na_mode" = '<NA>')

stat_mode_zone <- pel_dives %>%
  left_join(shelf_dives, by = "pol")

#ungroup() %>%
#group_by(pol) %>%
# reframe(mean_pel = mean(perc_shelf, na.rm = T),
#         sd_pel = sd(perc_shelf, na.rm = T))

stat_max_depth <- dives %>%
  group_by(dive_mode, pol) %>%
  reframe(mean_max_depth = mean(MAX_DEP), 
          sd_max_depth = sd(MAX_DEP)) %>%
  group_by(dive_mode, pol) %>%
  reframe(max_depth = sprintf("%.2f ± %.2f", mean_max_depth, sd_max_depth)) %>%
  ungroup() %>%
  spread(key = dive_mode, value = max_depth)

stat <- stat_max_depth %>%
  left_join(shelf_dives, by = "pol") %>%
  left_join(pel_dives, by = "pol")

write.csv(stat_max_depth, "output_data/stat_dives.csv")