## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-18
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
library(spatialrisk)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/useful_functions/assign_ctd_dt.R")
source("~/Desktop/NASA-hotspots/useful_functions/assign_ctd.R")
## ---------------------------

ctd <- readRDS("ctd_stations_table_north_bound_interp")
dives <- readRDS("dive_metrics_V8")

#==================================================================
# 1) METHOD 1: only time interval criterion
#==================================================================

dt = 2 #h: time interval threshold b/w CTD station and dives

seals <- dives %>%
  select(REF) %>%
  unique() %>%
  pull(REF) #___get seals ids

tab <- NULL
for (seal in seals) {
  print(seal)
  ctd_seal <- ctd %>%
    filter(REF == seal)
  
  dives_seal <- dives %>%
    filter(REF == seal)
  
  res <- assign_ctd_dt(ctd_seal, dives_seal, dt_max = dt)
  tab <- rbind(tab, res)
}

tab %>%
  filter(!is.na(id_ctd)) %>%
  summarise(n = n()) / nrow(dives) * 100 # % of dives with CTD

id_dive_ctd <- tab %>%
  filter(!is.na(id_ctd)) %>%
  select(c(REF, NUM, id_ctd)) 

dives_ctd <- dives %>%
  left_join(id_dive_ctd, by = c("REF", "NUM")) %>%
  filter(!is.na(id_ctd)) #___add id_ctd column to all dives (with CTD and w/o CTD)

## Save dataset dives_ctd
saveRDS(dives_ctd, "~/Desktop/WHOI/Data/output_data/dive_metrics_Vtest_ctd") 
  
#------------------------------------------------------------------
# COMPUTE SUMMARY ON METHOD FOR DIVES IN POLYNYAS
#------------------------------------------------------------------
dives_ctd <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_Vtest_ctd")

nrow(dives_ctd) / nrow(dives)

s_pol_ctd <- dives_ctd %>%
  filter(pol > 0) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring")) %>%
  group_by(pol, month, year) %>%
  summarise(n_dives_ctd = n())

s_pol_all <- dives %>%
  filter(pol > 0) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring")) %>%
  group_by(pol, month, year) %>%
  summarise(n_dives = n())

s_pol_all <- s_pol_all %>%
  left_join(s_pol_ctd, by = c("pol", "month", "year"))

s_pol_all$n_dives_ctd[is.na(s_pol_all$n_dives_ctd)] = 0

s_pol_all <- s_pol_all %>%
  mutate(perc = (n_dives - n_dives_ctd) / n_dives*100)

s_pol_all %>%
  ungroup() %>%
  filter(perc < 50) %>%
  summarise(n = n()) #___number of ob

hist(s_pol_all$perc, breaks = 30)

nobs <- summary_pol_season_all %>%
  group_by(pol, month) %>%
  reframe(n_obs = paste0("n=", n()),
          y = 0)

ggplot() +
  geom_boxplot(data = summary_pol_season_all, aes(x = factor(pol), group = factor(pol), y = fraction)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y + 5, label = n_obs), color = "darkgrey") +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  ylab("Percentage of dives with CTD") +
  xlab("Polynya id") +
  facet_grid(month~.) +
  theme_dark()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"))+
  geom_hline(yintercept = 50, col = "red")

ggsave("~/Dropbox/data/outputs_Marthe_2023/ctd/boxplot_percentage_dive_ctd.png",
       height = 50, width = 40, units = "cm", dpi = 300)

#------------------------------------------------------------------
# METHOD 2: two criteria (distance and time interval)
#------------------------------------------------------------------

seals <- dives %>%
  select(REF) %>%
  unique() %>%
  pull(REF)

max_dist <- 5000 #___(m) maximal distance between CTD and dive
tab_ctd_tot <- NULL #___df with result from selection on distance and timing between dive and CTD station

## Step 1: get all the dives in a given perimeter around each CTD
for (seal in seals) {
  print(seal)
  
  ctd_seal <- ctd %>%
    filter(REF == seal) #___ctd associated to one seal
  
  dives_seal <- dives %>%
    filter(REF == seal) #___dives associated to one seal
  
  if (nrow(ctd_seal) > 0) {
    tab_ctd_seal <- assign_ctd(ctd_seal, dives_seal, max_dist)
    tab_ctd_tot <- rbind(tab_ctd_tot, tab_ctd_seal)
  }
}

thr_time = 6 #h

tab_ctd_filt <- tab_ctd_tot %>%
  filter(max_depth_ctd >= MAX_DEP) %>%
  #filter(distance_m < 5000) %>%
  group_by(REF, NUM) %>%
  arrange(dist_t, .by_group = TRUE) %>%
  filter(row_number() == 1) %>%
  filter(dist_t <= thr_time * 60) #___min

nrow(tab_ctd_filt) / nrow(dives) * 100 #___fraction of dives with a ctd station

hist(tab_ctd_filt$distance_m)
hist(as.numeric(tab_ctd_filt$dist_t))

id_dive_ctd <- tab_ctd_filt %>%
  filter(!is.na(id_ctd)) %>%
  select(c(REF, NUM, id_ctd)) 

dives_ctd <- dives %>%
  left_join(id_dive_ctd, by = c("REF", "NUM")) %>%
  filter(!is.na(id_ctd)) #___add id_ctd column to all dives (with CTD and w/o CTD)

#------------------------------------------------------------------
# COMPUTE SUMMARY ON METHOD FOR DIVES IN POLYNYAS
#------------------------------------------------------------------
dives_ctd <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_test_2crit_ctd")

nrow(dives_ctd) / nrow(dives) * 100

s_pol_ctd <- dives_ctd %>%
  filter(pol > 0) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring")) %>%
  group_by(pol, month, year) %>%
  summarise(n_dives_ctd = n())

s_pol_all <- dives %>%
  filter(pol > 0) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring")) %>%
  group_by(pol, month, year) %>%
  summarise(n_dives = n())

s_pol_all <- s_pol_all %>%
  left_join(s_pol_ctd, by = c("pol", "month", "year"))

s_pol_all$n_dives_ctd[is.na(s_pol_all$n_dives_ctd)] = 0

s_pol_all <- s_pol_all %>%
  mutate(perc = (n_dives - n_dives_ctd) / n_dives*100)

s_pol_all %>%
  ungroup() %>%
  filter(perc < 50) %>%
  summarise(n = n()) #___number of ob

hist(s_pol_all$perc, breaks = 40)

nobs <- s_pol_all %>%
  group_by(pol, month) %>%
  reframe(n_obs = paste0("n=", n()),
          y = 0)

ggplot() +
  geom_boxplot(data = s_pol_all, aes(x = factor(pol), group = factor(pol), y = perc)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y + 5, label = n_obs), color = "darkgrey") +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  ylab("Percentage of dives with CTD") +
  xlab("Polynya id") +
  facet_grid(month~.) +
  theme_dark()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"))+
  geom_hline(yintercept = 50, col = "red")

## Save dataset dives_ctd
saveRDS(dives_ctd, "~/Desktop/WHOI/Data/output_data/dive_metrics_test_2crit_ctd") 

## End script
rm(list=ls())
