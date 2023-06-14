## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-13
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
## ---------------------------
## Library
library(dplyr)
## ---------------------------

## Palette polynyas
source("~/Desktop/WHOI/Codes/palettes/palette_polynya.R") #___palette polynyas (1-21)

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- as.list(polynya_info[,2]) #___as dictionnary (fill labels)
names(polynya_info_dict) = polynya_info$ID

dives <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_V8")
dives <- dives %>%
  mutate(month = month(DE_DATE),
         year = year(DE_DATE)) #___add month and year to dives

#==================================================================
# SUMMARY POLYNYA VISITS
#==================================================================

summary_seal <- dives %>%
  filter(pol > 0) %>%
  select(c(REF, pol)) %>%
  distinct()

#==================================================================
# NUMBER OF SEAL VISITING EACH POLYNYA
#==================================================================

summary_pol <- dives %>%
  filter(pol > 0) %>%
  select(c(REF, pol)) %>%
  group_by(pol) %>%
  distinct() %>%
  summarise(n_seals = n())

plot_nseals_by_pol <- 
  ggplot(summary_pol, aes(x = factor(pol), fill = factor(pol))) + 
  geom_bar(aes(y = n_seals), stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal, name = "Polynyas", labels = polynya_info_dict)+
  scale_y_continuous(n.breaks = 10) +
  xlab("Polynya")+
  ylab("Number of seals")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )

ggsave(plot = plot_nseals_by_pol, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/barplot_seals_per_pol.png", height = 30, width = 25, units = c("cm"), dpi = 300)


#==================================================================
# NUMBER OF DIVES BY POLYNYA
#==================================================================

pol_usage <- dives %>%
  filter(pol > 0) %>%
  group_by(pol, month) %>%
  summarise(n_dives = n()) #___only visited polynyas!

plot_ndives_by_pol <- 
  ggplot(pol_usage, aes(x = factor(pol), fill = factor(pol))) + 
  geom_bar(aes(y = n_dives), stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal, name = "Polynyas", labels = polynya_info_dict)+
  scale_y_continuous(n.breaks = 10) +
  xlab("Polynya")+
  ylab("Number of dives")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )

ggsave(plot = plot_ndives_by_pol, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/barplot_dives_per_pol.png", height = 30, width = 25, units = c("cm"), dpi = 300)


#==================================================================
# NUMBER OF DIVES BY POLYNYA BETWEEN JUNE AND OCTOBER
#==================================================================

pol_usage_w <- pol_usage %>%
  filter(month >= 6 & month <= 10)

plot_ndives_by_pol_w <- 
  ggplot(pol_usage_w, aes(x = factor(pol), fill = factor(pol))) + 
  geom_bar(aes(y = n_dives), stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal, name = "Polynyas", labels = polynya_info_dict)+
  scale_y_continuous(n.breaks = 10) +
  xlab("Polynya")+
  ylab("Number of dives")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )

ggsave(plot = plot_ndives_by_pol_w, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/June-Oct_barplot_dives_per_pol.png", height = 30, width = 25, units = c("cm"), dpi = 300)


#==================================================================
# HEATMAP OF TIME SPENT IN POLYNYAS
#==================================================================

summary_time_spent <- dives %>% 
  filter(pol > 0) %>%
  group_by(pol, day = yday(DE_DATE)) %>% 
  summarize(n_dives = n())

heatmap_time_pol <-
  ggplot(summary_time_spent, aes(day, as.factor(pol))) + 
  geom_tile(aes(fill = n_dives)) +
  scale_fill_viridis_c() +
  scale_y_discrete(labels = polynya_info_dict) +
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30)) +
  xlab("Day of the year") +
  ylab("") +
  labs(fill = "Number of dives") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(plot = heatmap_time_pol, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/PERIOD_COVERED_polynyas.png", height = 25, width = 50, units = c("cm"), dpi = 300)

#==================================================================
# PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA BY MONTH (all seals)
#==================================================================
threshold = 10 # days of data
file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/PERIOD_BY_NDAYS_polynyas_thr%s.png", threshold)

summary_time_spent_perc <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month) %>%
  select(c(pol, month, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  mutate(n_days_thresh = case_when(n_days < threshold ~ NA,
                                     n_days >= threshold ~ n_days))

heatmap_time_rel_pol <- 
  ggplot(summary_time_spent_perc, aes(x = factor(month), as.factor(pol))) + 
  geom_tile(aes(fill = n_days_thresh)) +
  scale_fill_viridis_c(na.value = "lightgrey") +
  scale_y_discrete(labels = polynya_info_dict) +
  xlab("Month") +
  ylab("") +
  labs(fill = "Number of days with data") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(plot = heatmap_time_rel_pol, file_name, height = 25, width = 40, units = c("cm"), dpi = 300)


#===============================================================================
# MIN AND MAX PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA BY MONTH (all seals)
#===============================================================================
threshold = 10 # days of data
file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/MIN_PERIOD_BY_NDAYS_polynyas_thr%s.png", threshold)

summary_time_spent_perc <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month, year) %>%
  select(c(pol, month, year, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  ungroup() %>%
  group_by(pol, month) %>%
  mutate(min_n_days = min(n_days, na.rm = T),
         max_n_days = max(n_days, na.rm = T)) %>%
  mutate(n_days_thr_min = case_when(min_n_days < threshold ~ NA,
                                    min_n_days >= threshold ~ min_n_days),
         n_days_thr_max = case_when(max_n_days < threshold ~ NA,
                                    max_n_days >= threshold ~ max_n_days))

## Min time period covered
heatmap_min_time_pol <- 
  ggplot(summary_time_spent_perc, aes(x = factor(month), as.factor(pol))) + 
  geom_tile(aes(fill = n_days_thr_min)) +
  scale_fill_viridis_c(na.value = "lightgrey") +
  scale_y_discrete(labels = polynya_info_dict) +
  xlab("Month") +
  ylab("") +
  labs(fill = "Number of days with data") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(plot = heatmap_min_time_pol, file_name, height = 20, width = 40, units = c("cm"), dpi = 300)

## Max time period covered
file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/MAX_PERIOD_BY_NDAYS_polynyas_thr%s.png", threshold)

heatmap_max_time_pol <- 
  ggplot(summary_time_spent_perc, aes(x = factor(month), as.factor(pol))) + 
  geom_tile(aes(fill = n_days_thr_max)) +
  scale_fill_viridis_c(na.value = "lightgrey") +
  scale_y_discrete(labels = polynya_info_dict) +
  xlab("Month") +
  ylab("") +
  labs(fill = "Number of days with data") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(plot = heatmap_max_time_pol, file_name, height = 20, width = 40, units = c("cm"), dpi = 300)


#==============================================================================
# PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA BY MONTH AND BY YEAR (all seals)
#==============================================================================

## End script
rm(list=ls())
