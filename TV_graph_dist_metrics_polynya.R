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
## Palette
source("~/Desktop/NASA-hotspots/palettes/palette_polynya.R")
source("~/Desktop/NASA-hotspots/palettes/palette_season.R")
## ---------------------------

## Import dives data
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0) %>%
  filter(!is.na(dive_mode))

dives <- dives %>%
  mutate(month = month(DE_DATE),
         season = case_when(month <= 2 ~ 1,
                            month >= 3 & month <= 5 ~ 2,
                            month >= 6 & month <= 8 ~ 3,
                            .default = 4)) %>%
  filter(season != 4)

## Import polynyas info
polynya_info <- read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
names_pol <- paste0(polynya_info$ID, " | ", polynya_info$Name)
names(names_pol) = polynya_info$ID

## Seasons
seasons <- c("Summer", "Autumn", "Winter", "Spring")
names(seasons) = 1:4

## Breaks depth
max_depth <- max(dives$MAX_DEP)
breaks_depth = seq(-max_depth, 0, 100)

#==================================================================
# 1) STATISTICS DIVES
#==================================================================

## Statistics
pel_dives <- dives %>%
  group_by(pol) %>%
  count(dive_mode) %>%
  mutate(ndives = sum(n)) %>%
  mutate(perc_pel = n/ndives*100) %>%
  filter(dive_mode == "pelagic") %>%
  dplyr::select(c(pol, perc_pel))

shelf_dives <- dives %>%
  group_by(pol) %>%
  count(zone) %>%
  mutate(ndives = sum(n)) %>%
  mutate(perc_shelf = n/ndives*100) %>%
  filter(zone == "shelf") %>%
  dplyr::select(c(pol, perc_shelf))

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

#==================================================================
# 5) DENSITY PLOT MLD
#==================================================================

mld_stations <- readRDS("ctd_stations_table_polynyas_MLD")

mld_stations <- mld_stations %>%
  mutate(month = month(time),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = NA)) %>%
  filter(!is.na(season))

summary_mld <- mld_stations %>%
  mutate(MLD = case_when(is.na(MLD) ~ 1000,
                         .default = MLD)) %>%
  group_by(pol, season) %>%
  reframe(mean = mean(MLD),
          median = median(MLD),
          Q1 = quantile(MLD, .25),
          Q3 = quantile(MLD, .75))

p6 <- ggplot(mld_stations) +
  geom_density(aes(y = -MLD, col = factor(season)), alpha = .6, linewidth = .5) +
  theme_bw() +
  xlab("Density") +
  facet_wrap(~pol, scales = "free_x",
             labeller = labeller(pol = names_pol), ncol = 6) +
  ylab("Mixed layer depth (m)")+
  labs(col = "Season") +
  scale_color_manual(values = pal_season, labels = seasons, na.value = NA) +
  theme(strip.text= element_text(size = 15),
        panel.spacing=unit(1.5, "lines")) +
  scale_y_continuous(breaks = breaks_depth)


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_MLD_pol_season.pdf",
    height = 10,
    width = 15)
print(p6)
print(p6 + ylim(c(-500, 0)))
dev.off()

#------------------------------------------------------------------
# HEATMAP
#------------------------------------------------------------------

mld_cells <- mld_stations %>%
  select(c(id_ctd, time, season, max_depth, MLD, pol))

range(mld_cells$MLD, na.rm = T)

mld_cells$bins <- cut(mld_cells$MLD, 
                      breaks = seq(0, 1000, 30),
                      include.lowest = TRUE)

nbins = nlevels(mld_cells$bins)
levels = levels(mld_cells$bins)
mld_cells <- mld_cells %>%
  mutate(bins = case_when(is.na(bins) ~ factor("> 1000"),
                          .default = bins))

mld_cells$bins <- factor(mld_cells$bins, levels = c(levels, "> 1000"))
labels_bins = rev(sort(unique(mld_cells$bins)))

mld_cells$bins <- as.numeric(mld_cells$bins)

n_season <- mld_cells %>%
  group_by(pol, season) %>%
  mutate(nseason = n()) %>%
  ungroup() %>%
  group_by(pol, season, bins, nseason) %>%
  mutate(nbin = n()) %>%
  reframe(frac = nbin/nseason, n = nbin) %>%
  unique()

nobs <- n_season %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", sum(n)),
          y = 1)

nobs$season <- factor(nobs$season, levels = c("Summer", "Autumn", "Winter"))
n_season$season <- factor(n_season$season, levels = c("Summer", "Autumn", "Winter"))

heatmap <- ggplot() +
  geom_tile(data = n_season, aes(x = factor(pol), y = factor(-bins), fill = frac)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y, label = n_obs), 
            color = "darkgrey", size = 3,  angle = 90, hjust = 0) +
  facet_grid(~season) +
  xlab("Polynya ids") +
  ylab("Mixed layer depth (m)") +
  scale_y_discrete(labels = labels_bins) +
  scale_fill_gradient2("Fraction", low = "white", high = "midnightblue", mid = "beige",
                       midpoint = 0.05, limit = c(0,1)) +
  theme_bw()

png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/heatmap_MLD_pol_season.png",
    height = 10,
    width = 25, units = "cm", res = 300)
print(heatmap)
dev.off()

#==================================================================
# 1) DENSITY DISTRIBUTION OF MAX DEPTH
#==================================================================

#------------------------------------------------------------------
# VIOLIN PLOT PELAGIC / BENTHIC DIVES
#------------------------------------------------------------------

dives_season_mode <- dives %>%
  mutate(month = month(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = NA)) %>%
  filter(!is.na(dive_mode) & !is.na(season))

dives_season_mode$season = factor(dives_season_mode$season, levels = seasons)

zone_mode_dives <- dives_season_mode %>%
  group_by(pol, zone) %>%
  reframe(med = median(MAX_DEP), Q1 = quantile(MAX_DEP, 0.25), Q3 = quantile(MAX_DEP, 0.75))

ggplot() +
  geom_boxplot(data = dives_season_mode, aes(x = factor(pol), y = -MAX_DEP, group = interaction(dive_mode, pol), 
                                             col = dive_mode))

#devtools::install_github("psyteachr/introdataviz")

library(introdataviz)
vp <- ggplot() +
  introdataviz::geom_split_violin(data = dives_season_mode, aes(x = 0, y = -MAX_DEP, fill = dive_mode),
                                  alpha = .4, trim = FALSE) +
  # geom_violin(data = dives_season_mode, aes(x = 0, y = -MAX_DEP, fill = dive_mode)) +
  facet_grid(pol~season, labeller = labeller(pol = names_pol), scales = "free_x") +
  theme_bw()

pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/violin_plot_max_depth_pol_season.pdf",
    height = 25,
    width = 7)
print(vp)
dev.off()

#------------------------------------------------------------------
# DENSITY DISTRIBUTION
#------------------------------------------------------------------

dives_season <- dives %>%
  mutate(month = month(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = NA))# %>%
  #filter(!(pol %in% c(1, 6, 7, 8, 15, 18, 19, 20)))

## Mean bathymetry
summary_bathy <- dives %>%
  group_by(pol) %>%
  reframe(mean = mean(bathy, na.rm = T),
          sd = sd(bathy, na.rm = T))

summary_mld <- summary_mld %>%
  filter(!(pol %in% c(1, 6, 7, 8, 15, 18, 19, 20)))

dives_season_bent <- dives %>%
  mutate(month = month(DE_DATE),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = NA)) %>%
  filter(dive_mode == "benthic")

nobs <- dives_season %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", n())) %>%
  mutate(y = case_when(season == "Summer" ~ -1200,
                       season == "Autumn" ~ -1100,
                       season == "Winter" ~ -1000))


dives_season$season = factor(dives_season$season, levels = seasons)
p1 <- ggplot(dives_season) +
  geom_density(aes(y = -MAX_DEP, col = factor(season)), alpha = .6, linewidth = 1) +
  geom_text(data = nobs, aes(x = Inf, y = y, label = n_obs, 
            color = factor(season)), size = 4, vjust = "inward", hjust = "inward") +
  #geom_hline(data = summary_mld, aes(yintercept = -mean, col = factor(season)), linetype = "dashed") +
  geom_hline(data = summary_bathy, aes(yintercept = mean)) +
  geom_hline(data = summary_bathy, aes(yintercept = mean - sd), linetype = "dashed") +
  #geom_hline(data = summary_bathy, aes(yintercept = mean + sd), linetype = "dashed") +
  theme_bw() +
  xlab("Density") +
  facet_wrap(~pol, scales = "free_x",
             labeller = labeller(pol = names_pol), nrow = 1) +
  ylab("Maximum dive depth (m)")+
  labs(col = "Season") +
  scale_color_manual(values = pal_season, labels = seasons, na.value = NA) +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth)
  #annotate(data = nobs, "text", x = 0.01, y = -700, aes(label = n_obs))
  
pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_max_depth_pol_season_bathy.pdf",
    height = 5,
    width = 40)
print(p1)
print(p1 + ylim(c(-500, 0)))
dev.off()


###### Try Darnley (9) and CP (17) hist bins
dives_test <- dives_season %>%
  filter(pol == 9 | pol == 17) %>%
  mutate(year = year(DE_DATE))

dives_test$bin <- cut(dives_test$MAX_DEP, 
    breaks = seq(0, max_depth, 1),
    include.lowest = TRUE) 

toto <- dives_test %>%
  group_by(bin, pol, year) %>%
  count()

toto <- 

ggplot() +
  geom_bar(data = toto, aes(x = bin, y = n),
           stat="identity", position="identity", color="grey") + 
  facet_grid(~pol) +
  coord_flip()

########### DENSITY
test <- dives_season %>%
  filter(pol == 10 & season == "Summer") %>%
  mutate(year = year(DE_DATE))
years = unique(year(test$DE_DATE))

test %>%
  group_by(year(DE_DATE)) %>%
  count()

test <- test %>%
  filter(!(year %in% c(2012)))

years = unique(test$year)

res = NULL
for (yr in years) {
  test_yr <- test %>%
    filter(year == yr)
  
  mod <- density(test_yr$MAX_DEP, kernel = "gaussian", from = 0, to = max_depth, n = max_depth/10, adjust = 1)
  df = tibble(depth = mod$x, density = mod$y, year = yr)
  res = rbind(res, df)
}

ggplot(data = test) +
  geom_density(aes(x = MAX_DEP, col = factor(year)))

sum <- res %>%
  group_by(depth) %>%
  reframe(med = median(density),
          Q1 = quantile(density, .25),
          Q3 = quantile(density, .75))

plot(sum$Q3, -sum$depth, type = "l", lty = 2)
lines(sum$Q1, -sum$depth, lty = 2)
lines(sum$med, -sum$depth, lty = 1)

ggplot(data = sum, aes(x = med, y = -depth)) +
  geom_ribbon(aes(xmin = Q1, xmax = Q3), fill = "grey70") +
  geom_path(aes(x = med, y = -depth))

dens <- density(test$MAX_DEP, kernel = "gaussian")

plot(dens$y, -dens$x, type = "l")

#------------------------------------------------------------------
# HEATMAP 
#------------------------------------------------------------------

cell_max_depth <- dives_season_pel %>%
  filter(!is.na(season)) %>%
  select(c(REF, NUM, dive_mode, DE_DATE, season, MAX_DEP, pol, zone))# %>%
  #filter(MAX_DEP >= 60)

cell_max_depth$bins <- cut(cell_max_depth$MAX_DEP, 
               breaks = seq(0, 1000, 30),
               include.lowest = TRUE) 

labels_bins = rev(sort(unique(cell_max_depth$bins)))

cell_max_depth$bins <- as.numeric(cell_max_depth$bins)

n_seals <- cell_max_depth %>%
  select(c(REF, pol, season)) %>%
  group_by(pol, season) %>%
  distinct() %>%
  reframe(nseals = paste0("nSES=", n()),
          y = 4)

n_season <- cell_max_depth %>%
  group_by(pol, season, zone) %>%
  mutate(nseason = n()) %>%
  ungroup() %>%
  group_by(pol, season, bins, nseason, zone) %>%
  mutate(nbin = n()) %>%
  reframe(frac = nbin/nseason, n = nbin) %>%
  unique()

n_season <- n_season %>%
  left_join(n_seals, by = c("pol", "season"))

nobs <- n_season %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", sum(n)),
          y = 1)

nobs$season <- factor(nobs$season, levels = c("Summer", "Autumn", "Winter"))
n_season$season <- factor(n_season$season, levels = c("Summer", "Autumn", "Winter"))
n_seals$season <- factor(n_seals$season, levels = c("Summer", "Autumn", "Winter"))

heatmap_max_depth <- ggplot() +
  geom_tile(data = n_season, aes(x = factor(pol), y = factor(-bins), fill = frac)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y, label = n_obs), 
            color = "darkgrey", size = 3,  angle = 90, hjust = 0) +
  geom_text(data = n_seals, aes(x = factor(pol), y = y, label = nseals), 
            color = "darkgrey", size = 3,  angle = 90, hjust = 0) +
  facet_grid(~season) +
  xlab("Polynya ids") +
  ylab("Maximum dive depth (m)") +
  scale_y_discrete(labels = labels_bins) +
  # scale_fill_gradient2("Fraction", low = "white", high = "darkred", mid = "red",
  #                      midpoint = 0.48, limit = c(0,1)) +
  scale_fill_gradient2("Fraction", low = "white", high = "darkblue", mid = "beige",
                       midpoint = 0.04, limit = c(0,1)) +
  theme_bw()

png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/heatmap/PELAGIC_heatmap_max_depth_pol_season.png",
    height = 15,
    width = 25, units = "cm", res = 300)
print(heatmap_max_depth)
dev.off()

#==================================================================
# 2) BOXPLOT HUNTING TIME
#==================================================================
dives_season_pel <- dives_season_pel %>%
  filter(!is.na(season)) %>%
  filter(MAX_DEP > 15) %>%
  mutate(yday = yday(DE_DATE))

range(dives_season_pel$MAX_DEP)

p2 <- ggplot() +
  geom_boxplot(data = dives_season_pel, aes(x = factor(month), y = hunting_time, fill = factor(season)), alpha = .6) +
  facet_wrap(~pol, labeller = labeller(pol = names_pol), nrow = 1) +
  theme_bw() +
  xlab("Month") +
  ylab("Fraction of dive in hunting mode")+
  labs(fill = "Season") +
  scale_fill_viridis_d(labels = seasons) +
  theme(strip.text= element_text(size = 20),
        panel.spacing=unit(1.5, "lines")) +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        # panel.grid.major.x = element_blank() ,
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )


#########
ggplot() +
  geom_point(data = dives_season_pel, aes(x = DIVE_DUR, y = hunting_time, col = factor(season)), size = .2) +
  scale_color_manual(values = pal_season)

ggplot() +
  geom_point(data = long_seg_dives, aes(x = DIVE_DUR, y = hunting_time, col = factor(pol)), size = .2) +
  scale_color_manual(values = pal)
#########


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/boxplot_HT_pol_month.pdf",
    height = 5,
    width = 35)
print(p2)
dev.off()


#==================================================================
# 3) DEPTH LONGEST SEGMENTS 
#==================================================================

long_seg <- readRDS("longer_segment_dives")

## Only keep segments for dives with CTD
long_seg_dives <- dives_season %>% #___only pelagic dives
  select(c(REF, NUM, DE_DATE, pol, season, month)) %>%
  full_join(long_seg, by = c("REF", "NUM")) %>%
  na.omit() %>%
  filter(!(pol %in% c(1, 6, 7, 8, 15, 18, 19, 20)))

## Season
p3 <- ggplot(long_seg_dives) +
  geom_density(aes(y = -mean_depth, col = factor(season)), alpha = .6, linewidth = 1) +
  geom_hline(data = summary_mld, aes(yintercept = -mean, col = factor(season)), linetype = "dashed") +
  theme_bw() +
  xlab("Density") +
  facet_wrap(~pol, scales = "free_x",
             labeller = labeller(pol = names_pol), nrow = 1) +
  ylab("Mean depth of longest segments (m)")+
  labs(col = "Season") +
  scale_color_manual(values = pal_season, labels = seasons, na.value = NA) +
  theme(strip.text= element_text(size = 15),
        panel.spacing=unit(1.5, "lines")) +
  scale_y_continuous(breaks = breaks_depth)


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/PELAGIC_density_depth_long_seg_pol_season.pdf",
    height = 5,
    width = 20)
print(p3)
print(p3 + ylim(c(-500, 0)))
dev.off()

## Month
p4 <- ggplot(long_seg_dives) +
  geom_density(aes(y = -mean_depth, col = factor(month)), alpha = .6, linewidth = .5) +
  theme_bw() +
  xlab("Density") +
  facet_wrap(~pol, scales = "free_x",
             labeller = labeller(pol = names_pol), ncol = 6) +
  ylab("Mean depth of longest segments (m)")+
  labs(col = "Season") +
  scale_color_viridis_d(labels = month.name) +
  theme(strip.text=element_text(margin=margin()),
        panel.spacing=unit(1.5, "lines"))


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_depth_long_seg_pol_month.pdf",
    height = 10,
    width = 15)
print(p4)
print(p4 + ylim(c(-500, 0)))
dev.off()

#------------------------------------------------------------------
# HEATMAP 
#------------------------------------------------------------------

cell_long_seg <- long_seg_dives %>%
  filter(!is.na(season)) %>%
  select(c(REF, NUM, DE_DATE, season, mean_depth, time_longer_seg, pol))

range(cell_long_seg$mean_depth)

cell_long_seg$bins <- cut(cell_long_seg$mean_depth, 
                           breaks = seq(0, 1000, 30),
                           include.lowest = TRUE) 

labels_bins = rev(sort(unique(cell_long_seg$bins)))

cell_long_seg$bins <- as.numeric(cell_long_seg$bins)

n_season <- cell_long_seg %>%
  group_by(pol, season) %>%
  mutate(nseason = n()) %>%
  ungroup() %>%
  group_by(pol, season, bins, nseason) %>%
  mutate(nbin = n()) %>%
  reframe(frac = nbin/nseason, n = nbin) %>%
  unique()

nobs <- n_season %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", sum(n)),
          y = 1)

nobs$season <- factor(nobs$season, levels = c("Summer", "Autumn", "Winter"))
n_season$season <- factor(n_season$season, levels = c("Summer", "Autumn", "Winter"))

heatmap_long_seg <- ggplot() +
  geom_tile(data = n_season, aes(x = factor(pol), y = factor(-bins), fill = frac)) +
  geom_text(data = nobs, aes(x = factor(pol), y = y, label = n_obs), 
            color = "darkgrey", size = 3,  angle = 90, hjust = 0) +
  facet_grid(~season) +
  xlab("Polynya ids") +
  ylab("Mean depth of longest segments (m)") +
  scale_y_discrete(labels = labels_bins) +
  scale_fill_gradient2("Fraction", low = "white", high = "darkblue", mid = "beige",
                       midpoint = 0.05, limit = c(0,0.73)) +
  theme_bw()

png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/PELAGIC_heatmap_long_seg_pol_season.png",
    height = 10,
    width = 25, units = "cm", res = 300)
print(heatmap_long_seg)
dev.off()


#==================================================================
# 4) BOXPLOT DIVE DURATION
#==================================================================

p5 <- ggplot() +
  geom_boxplot(data = dives_season, aes(x = factor(month), y = DIVE_DUR/60, fill = factor(season)), alpha = .6) +
  facet_wrap(~pol, labeller = labeller(pol = names_pol), nrow = 2) +
  theme_bw() +
  xlab("Month") +
  ylab("Dive duration (in min)")+
  labs(fill = "Season") +
  scale_fill_viridis_d(labels = seasons) +
  theme(strip.text= element_text(size = 20),
        panel.spacing=unit(1.5, "lines")) +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        # panel.grid.major.x = element_blank() ,
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/boxplot_dive_dur_pol_month.pdf",
    height = 10,
    width = 35)
print(p5)
dev.off()


## End script
rm(list=ls())

