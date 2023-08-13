## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-27
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
source("~/Desktop/NASA-hotspots/palettes/palette_polynya.R")
## ---------------------------

## Selected polynyas
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")
selection

## Import polynyas info
polynya_info <- read.csv("polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
names_pol <- paste0(polynya_info$ID, "\n", polynya_info$Name)
names(names_pol) = polynya_info$ID

## Hunting segments table
HS <- readRDS("behavioural_data/hunting_time_segments")

## Dives in polynyas
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0) %>%
  filter(!is.na(dive_mode)) %>%
  dplyr::select(c(REF, NUM, DE_DATE, pol, dive_mode, zone, bathy, id_ctd)) %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE)) %>%
  filter(month <= 8) #___summer-winter

## Add time, polynya id, dive mode and zone to hunting segments table
HS <- HS %>% left_join(dives, by = c("REF", "NUM")) %>%
  na.omit() %>%
  filter(pol %in% selection)


cell_HS <- HS %>%
  dplyr::select(c(REF, NUM, mean_depth_HS, pol))

max_depth = max(cell_HS$mean_depth_HS)
hist(cell_HS$mean_depth_HS)

size_bin <- 40 # m
lim_bin_max <- 1000 # m

cell_HS$bins <- cut(cell_HS$mean_depth_HS,
                    breaks = seq(0, lim_bin_max, size_bin))

lab <- rev(seq(0 + size_bin/2, lim_bin_max, size_bin))

labels_bins = rev(sort(unique(cell_HS$bins)))

cell_HS$bins <- as.numeric(cell_HS$bins)

summary <- cell_HS%>%
  group_by(pol) %>%
  mutate(n_pol = n()) %>%
  ungroup() %>%
  group_by(pol, bins) %>%
  mutate(nbin = n()) %>%
  reframe(frac = nbin/n_pol, n = nbin, ntot = paste0("n=", n_pol), y = factor(-max(cell_HS$bins))) %>%
  unique()

## Mean bathymetry
summary_bathy <- HS %>%
  group_by(pol) %>%
  reframe(mean = mean(bathy, na.rm = T),
          sd = sd(bathy, na.rm = T),
          median = median(bathy, na.rm = T),
          Q1 = quantile(bathy, .25),
          Q3 = quantile(bathy, .75))

summary_bathy$bins_med <- cut(-summary_bathy$median,
             breaks = seq(0, lim_bin_max, size_bin))
summary_bathy$bins_med <- as.numeric(summary_bathy$bins_med)

summary_bathy$bins_Q1 <- cut(-summary_bathy$Q1,
                              breaks = seq(0, lim_bin_max, size_bin))
summary_bathy$bins_Q1 <- as.numeric(summary_bathy$bins_Q1)

summary_bathy$bins_Q3 <- cut(-summary_bathy$Q3,
                             breaks = seq(0, lim_bin_max, size_bin))
summary_bathy$bins_Q3 <- as.numeric(summary_bathy$bins_Q3)


max_f <- max(summary$frac)
heatmap <- ggplot() +
  geom_tile(data = summary, aes(x = factor(pol), y = factor(-bins), fill = frac)) +
  geom_text(data = summary, aes(x = factor(pol), y = y, label = ntot), 
            color = "darkgrey", size = 3) + #,  angle = 90, hjust = 0) +
  geom_hline(data = summary_bathy, mapping = aes(yintercept = factor(-bins_med)), linewidth = .2) +
  geom_hline(data = summary_bathy, mapping = aes(yintercept = factor(-bins_Q1)), lty = 2, linewidth = .2) +
  geom_hline(data = summary_bathy, mapping = aes(yintercept = factor(-bins_Q3)), lty = 2, linewidth = .2) +
  facet_grid(~pol, scales = "free_x", labeller = labeller(pol = names_pol)) +
  ylab("Mean depth of foraging segments (m)") +
  scale_y_discrete(labels = lab) +
  scale_x_discrete(labels = NULL) +
  xlab("") +
  scale_fill_gradient2("Fraction", low = "white", high = "darkblue", mid = "beige",
                       midpoint = 0.001, limit = c(0, max_f)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.spacing = unit(-.2, "cm")) +
  theme(panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA))

heatmap

png("~/Dropbox/data/outputs_Marthe_2023/dist_metrics_polynyas/heatmap/heatmap_foraging_depth_bathy_quantiles.png",
    height = 15,
    width = 25, units = "cm", res = 300)
print(heatmap)
dev.off()


## End script
rm(list=ls())
