## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-07
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
library(dplyr)
library(spatialrisk)
library(plotly)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/select_ctd.R")
source("~/Desktop/NASA-hotspots/useful_functions/assign_ctd.R")
source("~/Desktop/NASA-hotspots/useful_functions/dist_kmR.R")
## ---------------------------

## Import data
ctd <- readRDS("ctd_data/ctd_stations_table_north_bound_interp") #___CTD stations table
dives_all_var <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_8") #___dives tables

dives <- dives_all_var %>%
  dplyr::select(c(REF, NUM, MAX_DEP, DE_DATE, interpLon, interpLat, pol))

#------------------------------------------------------------------
# Check if every seal in CTD table is associated to a seal in dives table
#------------------------------------------------------------------

ref_ctd <- ctd %>%
  pull(REF) %>%
  unique()

ref_dives <- dives %>%
  pull(REF) %>%
  unique()

which(!ref_ctd %in% ref_dives) #___supposed to be == integer(0)

#==================================================================
# Speed inside polynyas / outside polynyas
#==================================================================

speed <- dives_all_var %>%
  mutate(inside_pol = case_when(pol > 0 ~ "yes",
                                .default = "no")) %>%
  dplyr::select(c(interpLat, interpLon, inside_pol, DE_DATE))

speed <- speed %>%
  mutate(dist_lat = abs(interpLat) - abs(lag(interpLat, default = interpLat[1])),
         dist_lon = abs(interpLon) - abs(lag(interpLon, default = interpLon[1])),
         dt = DE_DATE - lag(DE_DATE, default = DE_DATE[1]))

speed$dist_km <- mapply(dist_km, speed$dist_lat, speed$dist_lon)

speed <- speed %>%
  mutate(speed_kmh = dist_km / abs(as.numeric(dt)/3600))

speed$inside_pol = factor(speed$inside_pol, levels = c("yes", "no")) 

stats_speed <- speed %>%
  group_by(inside_pol) %>%
  reframe(mean = mean(speed_kmh, na.rm = T),
          sd = sd(speed_kmh),
          median = median(speed_kmh, na.rm = T),
          Q1 = quantile(speed_kmh, .25, na.rm = T),
          Q3 = quantile(speed_kmh, .75, na.rm = T))

dplot <- ggplot() +
  geom_density(data = speed, aes(x = speed_kmh, fill = inside_pol, col = inside_pol), alpha = .4) +
  geom_vline(data = stats_speed, aes(xintercept = median, col = inside_pol), lty = 2) +
  # geom_vline(data = stats_speed, aes(xintercept = Q1, col = inside_pol), lty = 3) +
  # geom_vline(data = stats_speed, aes(xintercept = Q3, col = inside_pol), lty = 3) +
  theme_minimal() +
  xlab(expression(Speed~(km.h^-1))) +
  scale_color_viridis_d("Zone", direction = 1, labels = c("inside", "outside")) +
  scale_fill_viridis_d("Zone", direction = 1, labels = c("inside", "outside")) +
  scale_x_continuous(limits = c(0, 20), n.breaks = 10)


ggsave(plot = dplot, "~/Dropbox/data/outputs_Marthe_2023/ctd/density_plot_speed_in_out.png", 
       height = 6, width = 15, units = "cm", dpi = 300)


rm(speed)
#==================================================================
# Assign CTD stations to dives
#==================================================================

#------------------------------------------------------------------
# Step 1: assign CTD stations to dives on distance criterion
#------------------------------------------------------------------

max_dist <- 5000 #___(m) maximal distance between CTD and dive
seals <- ref_dives
tab_ctd_tot <- NULL #___df with result from selection on distance and timing between dive and CTD station

i = 1

for (seal in seals) {
  sprintf("%s: %s", i, seal)
  
  ctd_seal <- ctd %>%
    filter(REF == seal) #___ctd associated to one seal
  
  dives_seal <- dives %>%
    filter(REF == seal) #___dives associated to one seal
  
  if (nrow(ctd_seal) > 0) {
    tab_ctd_seal <- assign_ctd(ctd_seal, dives_seal, max_dist)
    tab_ctd_tot <- rbind(tab_ctd_tot, tab_ctd_seal)
  }
  
  i = i + 1
}

saveRDS(tab_ctd_tot, "behavioural_data/dives_ctd_assigned_not_filt")

#------------------------------------------------------------------
# Step 2: filter CTD stations on time criterion and maximum depth
#------------------------------------------------------------------

#tab_ctd_tot <- readRDS("behavioural_data/dives_ctd_assigned_not_filt")

dt_thresh <- 6 #___(h) maximal time interval between CTD and dive

## Filter CTD on time and maximum depth criteria
tab_ctd_filt <- tab_ctd_tot %>%
  filter(max_depth_ctd >= MAX_DEP) %>%
  filter(dist_t <= dt_thresh * 60) #___min

## Count the number of ctd stations attributed to each dive 
counts <- tab_ctd_filt %>%
  filter(max_depth_ctd >= MAX_DEP) %>%
  group_by(REF, NUM) %>%
  filter(dist_t <= dt_thresh * 60) %>% #___minutes
  summarise(n_ctd = n())

## Select the closest CTD station in time
tab_ctd_filt <- tab_ctd_filt %>%
  group_by(REF, NUM) %>%
  arrange(dist_t, .by_group = TRUE) %>%
  filter(row_number() == 1)

{
  pdf("~/Dropbox/data/outputs_Marthe_2023/hist_filt_CTD_dives.png", height = 4, width = 6)
  print(hist(tab_ctd_filt$distance_m, breaks = 30, 
             main = "Distance between dive locations and CTD stations")) #___hist of distance between dive location and CTD station
  print(xlab("Distance (m)"))

  print(hist(as.numeric(tab_ctd_filt$dist_t), breaks = 30, 
             main = "Time interval between dive locations and CTD stations")) #___hist of time interval between dive location and CTD station
  print(xlab("Time (min)"))
  dev.off()
}


#------------------------------------------------------------------
# Step 3: add CTD stations id to dives table
#------------------------------------------------------------------

id_dive_ctd <- tab_ctd_filt %>%
  filter(!is.na(id_ctd)) %>%
  dplyr::select(c(REF, NUM, id_ctd)) #___for join with dives table

dives_ctd <- dives_all_var %>%
  left_join(id_dive_ctd, by = c("REF", "NUM"))

perc <- nrow(dives_ctd %>% filter(!is.na(id_ctd))) / nrow(dives_ctd) * 100 #___% of dives with CTD station

saveRDS(dives_ctd, "behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9")

#-----Save output
sink("summary_outputs/summary_assign_ctd_dives.txt")
cat(paste("Last modified :", Sys.Date()))
cat("\n")
cat("\n")
cat(sprintf("Percentage of dives with CTD station: %.2f%%", perc))
sink()
#----------------






##########

table <- ctd_seal %>%
  full_join(dives_seal, by = "REF", relationship = "many-to-many") %>%
  filter(MAX_DEP <= max_depth) %>%
  mutate(dist_t = abs(difftime(DE_DATE, time, "hours"))) %>%
  filter(dist_t <= dt * 60 * 60) %>%
  group_by(REF, NUM) %>%
  arrange(dist_t, .by_group = T) %>%
  filter(row_number() == 1)

table <- table %>%
  mutate(dist_km = dist_km(interpLat.x, interpLat.y, interpLon.x, interpLon.y))

fig <- plot_ly(table)
fig <- fig %>% add_markers(x = ~interpLon.y, y = ~interpLat.y, name = 'dive',mode = 'markers', )
fig <- fig %>% add_markers(x = ctd_seal$interpLon, y = ctd_seal$interpLat, name = 'ctd',mode = 'markers', color = factor(ctd_seal$station), size = 1)
fig
  
nrow(table) / nrow(dives_seal) * 100
range(table$dist_t)
range(table$dist_km)
hist(table$dist_km, breaks = 100)
hist(as.numeric(table$dist_t), breaks = 100)
########

### Mean speed inside polynyas / outside polynyas
speed <- dives %>%
  mutate(inside_pol = case_when(pol > 0 ~ "yes",
                                .default = "no")) %>%
  select(c(interpLat, interpLon, inside_pol, DE_DATE))


speed <- speed %>%
  mutate(dist_lat = abs(interpLat) - abs(lag(interpLat, default = interpLat[1])),
         dist_lon = abs(interpLon) - abs(lag(interpLon, default = interpLon[1])),
         dt = DE_DATE - lag(DE_DATE, default = DE_DATE[1]))

speed$dist_km <- mapply(dist_km, speed$dist_lat, speed$dist_lon)

speed <- speed %>%
  mutate(speed_kmh = dist_km / abs(as.numeric(dt)/3600))

ggplot() +
  geom_density(data = speed, aes(x = speed_kmh, col = inside_pol), alpha = .4) +
  theme_minimal() +
  scale_x_continuous(n.breaks = 10)

ggsave("~/Dropbox/data/outputs_Marthe_2023/ctd/density_plot_speed_in_out.png", height = 10, width = 15, units = "cm", dpi = 300)

summary(speed %>% filter(inside_pol == "no") %>% pull(speed_kmh))

#------------------------------------------------------------------
# Step 2: filter CTD stations on time interval and depth criteria
## For each dive, keep the closest ctd station in time
#------------------------------------------------------------------
rm(list=ls())

thr_dist = 5000 # m
thr_time = 6 # h

dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_8")
tab_ctd_tot <- readRDS("behavioural_data/dives_ctd_assigned_not_filt")

## Test one dive
tab_ctd_tot %>%
  filter(REF == "ct96-25-13" & NUM == 3860) %>%
  filter(max_depth_ctd >= MAX_DEP) %>%
  ungroup() %>%
  group_by(REF, NUM) %>%
  arrange(dist_t, .by_group=TRUE) #%>%
  filter(row_number() == 1) %>%
  filter(dist_t <= thr_time * 60)
#########  

tab_ctd_filt <- tab_ctd_tot %>%
    filter(distance_m < thr_dist) %>% #___m
    filter(max_depth_ctd >= MAX_DEP) %>%
    group_by(REF, NUM) %>%    
    arrange(dist_t, .by_group = TRUE) %>%
    filter(row_number() == 1) %>%
    filter(dist_t <= thr_time * 60) #___min

nrow(tab_ctd_filt) / nrow(dives) * 100 #___fraction of dives with a ctd station

## Count the number of ctd stations attributed to each dive 
counts <- tab_ctd_tot %>%
  group_by(REF, NUM) %>%
  filter(dist_t <= thr_time * 60) %>% #___minutes
  summarise(n_ctd = n())

tab_ctd_sub <- tab_ctd_filt %>%
  select(c(REF, NUM, station))

dives_id_ctd <- dives %>%
  left_join(tab_ctd_sub, by = c("REF", "NUM"))

saveRDS(dives_id_ctd, "behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9")

# ## Distance between dives ##############
# dist_deg <- sqrt(diff(dives$interpLat)^2 + diff(dives$interpLon)^2)
# mean_lat <- mean(dives$interpLat)
# 
# library(okara)
# dist_km <- d2km(dist_deg, base.latitude = mean_lat)
# r <- dist_km/2
# summary(r)
# mean(r)
# hist(r, 500)
# hist(log(r), 30)
# 
# # Log-normal distribution
# esp <- exp(mean(r) + (sd(r)^2)/2)
# var <- (exp(sd(r)^2) - 1) * exp(2 * mean(r) + sd(r)^2)
# log_r <- log(r)
# 
# IC_min <- mean(log_r) - 1.96 * (sd(log_r) / sqrt(nrow(dives)))
# IC_max <- mean(log_r) + 1.96 * (sd(log_r) / sqrt(nrow(dives)))
# 
# exp(IC_min)
# exp(IC_max)
# 
# hist(log(dist_km/2), breaks = 30)
# summary(log(dist_km/2))
# sd(log(dist_km/2))
# #########################################
# 
# ## Distance between ctd #################
# dist_deg <- sqrt(diff(ctd$interpLat)^2 + diff(ctd$interpLon)^2)
# mean_lat <- mean(ctd$interpLat)
# 
# dist_km <- d2km(dist_deg, base.latitude = mean_lat)
# r <- dist_km/2
# summary(r)
# mean(r)
# sd(r)
# hist(r, 500)

## Condition on distance:
# Q3 (0.75)
##########################################

## End script
rm(list=ls())
