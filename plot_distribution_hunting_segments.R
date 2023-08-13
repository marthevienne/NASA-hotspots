## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-24
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
library(lubridate)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/palettes/palette_season.R")
source("~/Desktop/NASA-hotspots/palettes/palette_polynya.R")
## ---------------------------

## Import polynyas info
polynya_info <- read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
names_pol <- paste0(polynya_info$ID, " | ", polynya_info$Name)
names(names_pol) = polynya_info$ID

## Selected polynyas
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")
selection

## Hunting segments table
HS <- readRDS("behavioural_data/hunting_time_segments")

## Dives in polynyas
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0) %>%
  #filter(pol == 14) %>%
  filter(!is.na(dive_mode)) %>%
  dplyr::select(c(REF, NUM, DE_DATE, pol, dive_mode, zone, bathy, id_ctd)) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE)) %>%
  filter(month <= 8) %>%
  filter(pol %in% selection)
  

## Add time, polynya id, dive mode and zone to hunting segments table
HS <- HS %>% left_join(dives, by = c("REF", "NUM")) %>%
  na.omit()

## Add season
seasons = c("Summer", "Autumn", "Winter")
HS <- HS %>%
  mutate(season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring"))

## Filter years on # obs
rm_period <- HS %>%
  group_by(pol, season, year) %>%
  count() %>%
  filter(n < 30) %>%
  dplyr::select(c(pol, season, year)) %>%
  mutate(tmp_id = paste0(pol, season, year))

# HS <- HS %>%
#   mutate(tmp_id = paste0(pol, season, year)) %>%
#   filter(!(tmp_id %in% rm_period$tmp_id))

## Breaks depth
max_depth = max(HS$mean_depth_HS)
breaks_depth = seq(-1500, 0, 100)


## Estimate distribution of mean depth of foraging depths for each polynya
max_depth = max(HS$mean_depth_HS)
ids <- sort(unique(HS$pol))
res = NULL

for (id in ids) {
  
  HS_pol <- HS %>%
    filter(pol == id)

  if (nrow(HS_pol) > 1) {
    mod <- density(HS_pol$mean_depth_HS, kernel = "gaussian", from = 0, to = max_depth, n = max_depth/10, adjust = 1)
    df = tibble(depth = mod$x, density = mod$y, pol = id)
    res = rbind(res, df)
  }
}

## Mean bathymetry
summary_bathy <- HS %>%
  group_by(pol) %>%
  reframe(mean = mean(bathy, na.rm = T),
          sd = sd(bathy, na.rm = T),
          Q1 = quantile(bathy, .25),
          Q3 = quantile(bathy, .75))

## Number of observations
nobs <- HS %>%
  dplyr::select(c(pol)) %>%
  group_by(pol) %>%
  reframe(n_obs = paste0("n=", n()))

res$pol <- factor(res$pol, levels = sort(unique(res$pol), decreasing = T))

res_sample <- res %>%
  filter(pol %in% c(21))
p <- ggplot(data = res_sample, mapping = aes(x = density, y = -depth, col = factor(pol))) +
  geom_rect(data = summary_bathy, 
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -1000, ymax = mean),
            fill = "grey", alpha=.3, inherit.aes = F) +
  geom_path(aes(x = density, y = -depth), linewidth = 1) +
  geom_label(data = nobs, aes(x = Inf, y = -950, label = n_obs), size = 3, vjust = "inward", hjust = "inward", fill = "white", inherit.aes = F) +
  scale_color_manual(values = pal) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE), col = guide_legend(reverse = TRUE)) +
  xlab("Kernel density of hunting segments depths") +
  # facet_wrap(~pol,
  #            labeller = labeller(pol = names_pol), nrow = 2) +
  ylab("Maximum dive depth (m)")+
  labs(col = "Polynya") +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth, limits = c(-max_depth, 0))


p

## Mean bathymetry
summary_bathy <- HS %>%
  group_by(pol) %>%
  reframe(mean = mean(bathy, na.rm = T),
          sd = sd(bathy, na.rm = T),
          Q1 = quantile(bathy, .25),
          Q3 = quantile(bathy, .75))

## Mean MLD
MLD <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
  filter(!is.na(MLD)) %>%
  dplyr::select(c(id_ctd, MLD, time)) %>%
  mutate(month = month(time),
         season = case_when(month <= 2 ~ "Summer",
                             month >= 3 & month <= 5 ~ "Autumn",
                             month >= 6 & month <= 8 ~ "Winter",
                             .default = "Spring")) %>%
  filter(season != "Spring")

MLD <- MLD %>% left_join(HS[, c("id_ctd", "pol")], by = "id_ctd")

summary_MLD <- MLD %>%
  group_by(pol, season) %>%
  reframe(mean = mean(MLD, na.rm = T),
          sd = sd(MLD, na.rm = T),
          Q1 = quantile(MLD, .25),
          Q3 = quantile(MLD, .75)) %>%
  na.omit()


## For all polynyas
max_depth = max(HS$mean_depth_HS)
ids <- sort(unique(HS$pol))
res = NULL

for (id in ids) {
  
  HS_pol <- HS %>%
    filter(pol == id)
  
  years = unique(HS_pol$year)
  
  for (s in seasons) {
    for (yr in years) {
      HS_pol_yr <- HS_pol %>%
        filter(year == yr & season == s)
      
      if (nrow(HS_pol_yr) > 1) {
        mod <- density(HS_pol_yr$mean_depth_HS, kernel = "gaussian", from = 0, to = max_depth, n = max_depth/10, adjust = 1)
        df = tibble(depth = mod$x, density = mod$y, year = yr, season = s, pol = id)
        res = rbind(res, df)
      }
    }
  }
}

nyrs <- res %>%
  dplyr::select(c(pol, season, year)) %>%
  distinct() %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", n())) %>%
  mutate(y = case_when(season == "Summer" ~ -1300,
                       season == "Autumn" ~ -1200,
                       season == "Winter" ~ -1100))

nyrs$season <- factor(nyrs$season, levels = rev(seasons))

nobs <- HS %>%
  dplyr::select(c(pol, season, year)) %>%
  group_by(pol, season) %>%
  reframe(n_obs = paste0("n=", n())) %>%
  spread(key = season, value = n_obs) %>%
  ungroup() %>%
  group_by(pol) %>%
  mutate(label = sprintf("Summer: %s\nAutumn: %s\nWinter: %s", Summer, Autumn, Winter))

sum <- res %>%
  group_by(depth, season, pol) %>%
  reframe(
    med = median(density),
    Q1 = quantile(density, .25),
    Q3 = quantile(density, .75),
    mean = mean(density),
    min = min(density),
    max = max(density)
  )

toto <- res %>%
  filter(!(pol %in% c(1, 6, 7, 8, 18, 20))) %>%
  group_by(depth, pol, season) %>%
  filter(pol == 9 & season == "Winter") %>%
  filter(depth == 0)

mid_pol <- sum %>%
  group_by(pol) %>%
  summarise(mid = max(Q3)/2,
            size_bar = max(Q3)/10)

summary_bathy_annot <- summary_bathy %>%
  left_join(mid_pol, by = "pol")


sum$season <- factor(sum$season, levels = rev(seasons))


## With Mertz
p2 <- ggplot(data = sum, mapping = aes(x = med, y = -depth, col = season)) +
  geom_rect(data = summary_bathy, 
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -1000, ymax = mean),
            fill = "grey", alpha=.3, inherit.aes = F) +
  geom_hline(data = summary_bathy_annot, mapping = aes(yintercept = mean - sd), lty = 2, col = "grey")+ 
  geom_hline(data = summary_bathy_annot, mapping = aes(yintercept = mean + sd), lty = 2, col = "grey")+ 
  # geom_errorbar(data = summary_bathy_annot_wo_mertz, 
  #               mapping = aes(x = mid, ymin = mean - sd, ymax = mean + sd, width = size_bar), color="grey", inherit.aes = F)+
  geom_path(aes(x = med, y = -depth), linewidth = .7) +
  geom_ribbon(aes(xmin = Q1, xmax = Q3, fill = season), alpha = .4, colour = NA) +
  # geom_text(data = nobs, aes(x = Inf, y = y, label = n_obs,
  #               color = factor(season)), size = 4, vjust = -0.003, hjust = "inward", fill = "white") +
  geom_label(data = nobs, aes(x = Inf, y = -950, label = label), size = 3, vjust = "inward", hjust = "inward", fill = "white", inherit.aes = F) +
  scale_color_manual(values = pal_season) +
  scale_fill_manual(values = pal_season) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE), col = guide_legend(reverse = TRUE)) +
  xlab("Kernel density of hunting segments depths") +
  facet_wrap(~pol,
             labeller = labeller(pol = names_pol), nrow = 2) +
  ylab("Maximum dive depth (m)")+
  labs(col = "Season", fill = "Season") +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth, limits = c(-max_depth, 0))


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_HS_pol_season_bathy_with_Mertz.pdf",
    height = 12,
    width = 23)
print(p2 + scale_y_continuous(limits = c(-1000, 0), breaks = seq(-1000, 0, 50)))
print(p2 + scale_y_continuous(limits = c(-700, 0), breaks = seq(-700, 0, 50)))
dev.off()


## Remove Mertz and Cape Poinsett
sum_wo_mertz <- sum %>% filter(pol != 21 & pol != 17)
summary_bathy_annot_wo_mertz <- summary_bathy_annot %>% filter(pol != 21 & pol != 17)
nobs_wo_mertz <- nobs %>% filter(pol != 21 & pol != 17)
summary_MLD_wo_mertz <- summary_MLD %>% filter(pol != 21 & pol != 17)

p1 <- ggplot(data = sum_wo_mertz, mapping = aes(x = med, y = -depth, col = season)) +
  geom_rect(data = summary_bathy_annot_wo_mertz, 
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -1000, ymax = mean),
            fill = "grey", alpha=.3, inherit.aes = F) +
  geom_hline(data = summary_MLD_wo_mertz, mapping = aes(yintercept = -mean, col = season)) +
  geom_hline(data = summary_bathy_annot_wo_mertz, mapping = aes(yintercept = mean - sd), lty = 2, col = "grey")+ 
  geom_hline(data = summary_bathy_annot_wo_mertz, mapping = aes(yintercept = mean + sd), lty = 2, col = "grey")+ 
  # geom_errorbar(data = summary_bathy_annot_wo_mertz, 
  #               mapping = aes(x = mid, ymin = mean - sd, ymax = mean + sd, width = size_bar), color="grey", inherit.aes = F)+
  geom_path(aes(x = med, y = -depth), linewidth = .7) +
  geom_ribbon(aes(xmin = Q1, xmax = Q3, fill = season), alpha = .4, colour = NA) +
  # geom_text(data = nobs, aes(x = Inf, y = y, label = n_obs,
  #               color = factor(season)), size = 4, vjust = -0.003, hjust = "inward", fill = "white") +
  geom_label(data = nobs_wo_mertz, aes(x = Inf, y = -950, label = label), size = 3, vjust = "inward", hjust = "inward", fill = "white", inherit.aes = F) +
  scale_color_manual(values = pal_season) +
  scale_fill_manual(values = pal_season) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE), col = guide_legend(reverse = TRUE)) +
  xlab("Kernel density of hunting segments depths") +
  xlim(0, max(sum_wo_mertz$Q3))+
  facet_wrap(~pol,
             labeller = labeller(pol = names_pol), nrow = 2, scales = "free") +
  ylab("Maximum dive depth (m)")+
  labs(col = "Season", fill = "Season") +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth, limits = c(-max_depth, 0))


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_HS_pol_season_bathy.pdf",
    height = 12,
    width = 23)
print(p1 + scale_y_continuous(limits = c(-1000, 0), breaks = seq(-1000, 0, 50)))
print(p1 + scale_y_continuous(limits = c(-700, 0), breaks = seq(-700, 0, 50)))
dev.off()

## Mertz
sum_mertz <- sum %>% filter(pol == 21)
summary_bathy_annot_mertz <- summary_bathy_annot %>% filter(pol == 21)
nobs_mertz <- nobs %>% filter(pol == 21)

p_mertz <- ggplot(data = sum_mertz, mapping = aes(x = med, y = -depth, col = season)) +
  geom_rect(data = summary_bathy_annot_mertz, 
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -1000, ymax = mean),
            fill = "grey", alpha=.3, inherit.aes = F) +
  geom_hline(data = summary_bathy_annot_mertz, mapping = aes(yintercept = mean - sd), lty = 2, col = "grey")+ 
  geom_hline(data = summary_bathy_annot_mertz, mapping = aes(yintercept = mean + sd), lty = 2, col = "grey")+ 
  # geom_errorbar(data = summary_bathy_annot_wo_mertz, 
  #               mapping = aes(x = mid, ymin = mean - sd, ymax = mean + sd, width = size_bar), color="grey", inherit.aes = F)+
  geom_path(aes(x = med, y = -depth), linewidth = .7) +
  geom_ribbon(aes(xmin = Q1, xmax = Q3, fill = season), alpha = .4, colour = NA) +
  # geom_text(data = nobs, aes(x = Inf, y = y, label = n_obs,
  #               color = factor(season)), size = 4, vjust = -0.003, hjust = "inward", fill = "white") +
  geom_label(data = nobs_mertz, aes(x = Inf, y = -950, label = label), size = 3, vjust = "inward", hjust = "inward", fill = "white", inherit.aes = F) +
  scale_color_manual(values = pal_season) +
  scale_fill_manual(values = pal_season) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE), col = guide_legend(reverse = TRUE)) +
  xlab("Kernel density of hunting segments depths") +
  #xlim(0, max(sum_wo_mertz$Q3))+
  facet_wrap(~pol,
             labeller = labeller(pol = names_pol), nrow = 2, scales = "free") +
  ylab("Maximum dive depth (m)")+
  labs(col = "Season", fill = "Season") +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth, limits = c(-max_depth, 0))


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_HS_pol_season_bathy_mertz.pdf",
    height = 6,
    width = 4)
print(p_mertz + scale_y_continuous(limits = c(-1000, 0), breaks = seq(-1000, 0, 50)))
print(p_mertz + scale_y_continuous(limits = c(-700, 0), breaks = seq(-700, 0, 50)))
dev.off()


## Mean, min & max density
p3 <- ggplot(data = sum, mapping = aes(x = mean, y = -depth, col = season)) +
  geom_rect(data = summary_bathy_annot, 
            mapping = aes(xmin = -Inf, xmax = Inf, ymin = -1000, ymax = mean),
            fill = "grey", alpha=.3, inherit.aes = F) +
  geom_hline(data = summary_bathy_annot, mapping = aes(yintercept = mean - sd), lty = 2, col = "grey")+ 
  geom_hline(data = summary_bathy_annot, mapping = aes(yintercept = mean + sd), lty = 2, col = "grey")+ 
  # geom_errorbar(data = summary_bathy_annot_wo_mertz, 
  #               mapping = aes(x = mid, ymin = mean - sd, ymax = mean + sd, width = size_bar), color="grey", inherit.aes = F)+
  geom_path(aes(x = mean, y = -depth), linewidth = .7) +
  geom_ribbon(aes(xmin = min, xmax = max, fill = season), alpha = .4, colour = NA) +
  # geom_text(data = nobs, aes(x = Inf, y = y, label = n_obs,
  #               color = factor(season)), size = 4, vjust = -0.003, hjust = "inward", fill = "white") +
  geom_label(data = nobs, aes(x = Inf, y = -950, label = label), size = 3, vjust = "inward", hjust = "inward", fill = "white", inherit.aes = F) +
  scale_color_manual(values = pal_season) +
  scale_fill_manual(values = pal_season) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE), col = guide_legend(reverse = TRUE)) +
  xlab("Kernel density of hunting segments depths") +
  facet_wrap(~pol,
             labeller = labeller(pol = names_pol), nrow = 2, scales = "free_x") +
  ylab("Maximum dive depth (m)")+
  labs(col = "Season", fill = "Season") +
  theme(strip.text=element_text(size = 15),
        panel.spacing=unit(1.5, "lines"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "white")) +
  scale_y_continuous(breaks = breaks_depth, limits = c(-max_depth, 0))


pdf("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/density_HS_pol_season_bathy_mean_min_max.pdf",
    height = 12,
    width = 23)
print(p3 + scale_y_continuous(limits = c(-1000, 0), breaks = seq(-1000, 0, 50)))
print(p3 + scale_y_continuous(limits = c(-700, 0), breaks = seq(-700, 0, 50)))
dev.off()


#################### FOR LEGEND
toto <- sum %>%
  filter(pol == 9 & season == "Winter")
ggplot(data = toto, mapping = aes(x = med, y = -depth)) +
  geom_path(aes(x = med, y = -depth), linewidth = 1, col = "darkgrey")+
  geom_ribbon(aes(xmin = Q1, xmax = Q3), alpha = .4, colour = NA, fill = "grey") +
    theme_classic()+
  scale_y_continuous(limits = c(-800, 10))
#####################



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
  
  mod <- density(test_yr$mean_depth_HS, kernel = "gaussian", from = 0, to = max_depth, n = max_depth/10, adjust = 1)
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


## End script
rm(list=ls())
