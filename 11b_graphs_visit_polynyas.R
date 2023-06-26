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
library(wesanderson)
remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
## ---------------------------

## Palette polynyas
source("~/Desktop/WHOI/Codes/palettes/palette_polynya.R") #___palette polynyas (1-21)

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- polynya_info[,2] #___as dictionnary (fill labels)
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
# DATA COVERAGE BY POLYNYA
#==================================================================

#---Stacked barplot of number of days with data by polynya and by year

pol_usage_monthly <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month, year) %>%
  select(c(pol, month, year, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  mutate(season = case_when(month <= 2 ~ "Spring-Summer",
                            month >= 3 & month <= 5 ~"Autumn",
                            month > 5 & month <= 8 ~ "Winter"))

source("~/Desktop/WHOI/Codes/palettes/palette_years.R")

stack_barplot <- ggplot() +
  geom_bar(data = toto,
           aes(x = factor(month), y = n_days, fill = factor(year)), col = "white", linewidth = .2,
           position = "stack", stat = "identity") +
  facet_wrap(pol~., labeller = labeller(pol = polynya_info_dict), ncol = 6, nrow = 3) +
  xlab("Month")+
  ylab("Number of days of data")+
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  ) +
  scale_fill_manual(name = "Year", values = pal_years)

ggsave(plot = stack_barplot, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/ndays_per_pol_per_month.png", height = 30, width = 40, units = c("cm"), dpi = 300)

#---Barplot
barplot <- ggplot() +
  geom_bar(data = pol_usage_monthly,
           aes(x = factor(month), y = n_days, fill = factor(year)),
           stat = "identity") +
  facet_grid(pol~year, labeller = labeller(pol = polynya_info_dict)) +
  xlab("Month")+
  ylab("Number of days with data")+
  scale_y_continuous(breaks = seq(0, 31, 10)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = NA)
  ) +
  scale_fill_manual(name = "Year", values = pal_years)

ggsave(plot = barplot, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/ndays_per_pol_per_month_pol_year.png", height = 50, width = 80, units = c("cm"), dpi = 300)


#---Boxplot of number of days with data by polynya and by year (with # obs)

nobs <- pol_usage_monthly %>%
  group_by(pol, month) %>%
  reframe(n_obs = paste0("n=", n()),
          y = 30)

boxplot <- ggplot() +
  geom_boxplot(data = pol_usage_monthly, aes(x = factor(month), y = n_days, color = factor(month)), fill = "grey", alpha = .6) +
  geom_text(data = nobs, aes(x = factor(month), y = y + 5, label = n_obs), color = "darkgrey") +
  facet_wrap(pol~., labeller = labeller(pol = polynya_info_dict), ncol = 3) +
  xlab("Month")+
  ylab("Number of days of data") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"),
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )


ggsave(plot = boxplot, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/ndays_per_pol_per_month_boxplot.png", height = 30, width = 35, units = c("cm"), dpi = 300)


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

ggsave(plot = heatmap_time_pol, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/DATA_COVERAGE_polynyas.png", height = 25, width = 50, units = c("cm"), dpi = 300)

#========================================================================================================
# BY MONTH | PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA (continuous with threshold or not) (all seals)
#========================================================================================================
min_yr <- min(dives$year, na.rm = T)
max_yr <- max(dives$year, na.rm = T)

threshold = 0 # days of data
file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/DATA_COVERAGE_polynyas_thr%s.png", threshold)

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
  ggtitle(sprintf("%s-%s", min_yr, max_yr))+
  theme_bw() +
  theme(axis.text.y = element_text(size = 15, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"), 
        plot.title = element_text(size = 30, face = "bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(plot = heatmap_time_rel_pol, file_name, height = 20, width = 40, units = c("cm"), dpi = 300)

#==============================================================================
# BY MONTH | PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA (bins) (all seals)
#==============================================================================
bins = c("]0-5]", "]5-10]", "]10-15]", "]15-20]", "]20-25]", "]25-31]")
pal = c("#DEF5E5FF", "#60CEACFF", "#3497A9FF", "#395D9CFF", "#382A54FF", "#0B0405FF")

names(pal) = bins

summary <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month, year) %>%
  select(c(pol, month, year, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  reframe(mean_n_days = mean(n_days),
         min_n_days = min(n_days),
         max_n_days = max(n_days)) %>%
  mutate(cat_mean = case_when(mean_n_days > 25 ~ "]25-31]",
                         mean_n_days > 20 ~ "]20-25]",
                         mean_n_days > 15 ~ "]15-20]",
                         mean_n_days > 10 ~ "]10-15]",
                         mean_n_days > 5 ~ "]5-10]",
                         mean_n_days <= 5 ~ "[0-5]"),
         cat_min = case_when(min_n_days > 25 ~ "]25-31]",
                              min_n_days > 20 ~ "]20-25]",
                              min_n_days > 15 ~ "]15-20]",
                              min_n_days > 10 ~ "]10-15]",
                              min_n_days > 5 ~ "]5-10]",
                              min_n_days <= 5 ~ "[0-5]"),
         cat_max = case_when(max_n_days > 25 ~ "]25-31]",
                              max_n_days > 20 ~ "]20-25]",
                              max_n_days > 15 ~ "]15-20]",
                              max_n_days > 10 ~ "]10-15]",
                              max_n_days > 5 ~ "]5-10]",
                              max_n_days <= 5 ~ "[0-5]")
         )

head(summary)

summary$cat <- factor(summary_time_spent_perc_month$cat, levels = rev(bins))
labs <- c(rev(bins), "")

all_pol_summary <- polynya_info %>% left_join(summary, by = c("ID" = "pol")) #___include all polynyas, even those w/o data

month_df <- data_frame(month = rep(seq(1:12), 21), pol = rep(seq(1:21), each = 12))

all_pol_summary_all_month <- all_pol_summary %>%
  full_join(month_df, by = c("month", "ID" = "pol")) #___include all months, even those w/o data

head(all_pol_summary_all_month)

mode = "min"
file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/%s_DATA_COVERAGE_polynyas_bins.pdf", mode)

heatmap <- 
  ggplot(all_pol_summary_all_month, aes(x = factor(month), as.factor(ID))) + 
  scale_y_discrete(labels = polynya_info_dict) +
  scale_x_discrete(labels = seq(1:12), limits = factor(seq(1:12))) +
  xlab("Month") +
  ylab("") +
  ggtitle(sprintf("%s-%s", min_yr, max_yr)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        plot.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 20), #__________ legend texts
        legend.title = element_text(size = 20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")) +
  scale_fill_manual(values = pal, na.value = NA, limits = rev(bins)) #___limits -> display all bins in colorscale

if (mode == "mean") {
  heatmap <- heatmap +
    labs(fill = "Mean number of \ndays with data") +
    geom_tile(aes(fill = cat_mean), col = "white")
} else if (mode == "min") {
  heatmap <- heatmap +
    labs(fill = "Minimum number of \ndays with data") +
    geom_tile(aes(fill = cat_min), col = "white")
} else {
  heatmap <- heatmap +
    labs(fill = "Maximum number of \ndays with data") +
    geom_tile(aes(fill = cat_max), col = "white")
}
ggsave(plot = heatmap, file_name, height = 20, width = 40, units = c("cm"), dpi = 300)


#============================================================================================================
# BY MONTH/YEAR | PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA (continuous with threshold or not) (all seals)
#============================================================================================================
threshold = 15 # days of data
category = F

if (category) {
  file_name = sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/DATA_COVERAGE_by_year_polynyas_thr%s.pdf", threshold)
} else {
  file_name = "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/DATA_COVERAGE_by_year_polynyas.pdf"
}

summary_time_spent_perc_yr_month <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month, year) %>%
  select(c(pol, month, year, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  mutate(n_days_thresh = case_when(n_days < threshold ~ sprintf("< %s", threshold),
                                   n_days >= threshold ~ sprintf(">= %s", threshold)))

summary_time_spent_perc_yr_month %>%
  filter(n_days >= 15) %>%
  ungroup() %>%
  summarize(n = n())

years <- sort(unique(summary_time_spent_perc_yr_month$year))
pal <- c("#80CAAC", "#E5E5E5")
labs = c(sprintf('< %s', threshold), sprintf('>= %s', threshold), '')

pdf(file_name,
    height = 6,
    width = 15)

for (yr in years) {
  
  summary <- summary_time_spent_perc_yr_month %>%
    filter(year == yr)
  
  all_pol_summary <- polynya_info %>% left_join(summary, by = c("ID" = "pol")) #___include all polynyas, even those w/o data
  
  month_df <- data_frame(month = rep(seq(1:12), 21), pol = rep(seq(1:21), each = 12))
  all_pol_summary_all_month <- all_pol_summary %>% 
    full_join(month_df, by = c("month", "ID" = "pol")) #___include all months, even those w/o data
  
  heatmap <- 
    ggplot(all_pol_summary_all_month, aes(x = factor(month), as.factor(ID))) + 
    scale_y_discrete(labels = polynya_info_dict) +
    scale_x_discrete(labels = seq(1:12), limits = factor(seq(1:12))) +
    xlab("Month") +
    ylab("") +
    labs(fill = "Number of days \nwith data") +
    ggtitle(yr) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 20, face="bold"),
          axis.text.x = element_text(size = 20),
          axis.title = element_text(size = 25, face="bold"),
          plot.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 20), #__________ legend texts
          legend.title = element_text(size = 20, face="bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "white")) 
  
  if (category) {
    heatmap <- heatmap + 
      geom_tile(aes(fill = n_days_thresh), col = "white") +
      scale_fill_manual(values = rev(pal), na.value = NA, labels = labs)
  } else {
    heatmap <- heatmap +
      geom_tile(aes(fill = n_days)) +
      scale_fill_viridis_c(na.value = NA, breaks = seq(0, 31, 5), limits = c(0, 31)) +
      guides(fill = guide_colourbar(barwidth = 1, barheight = 10))
  }
  
  print(heatmap)
}

dev.off()

#=================================================================================
# BY MONTH/YEAR | PERCENTAGE OF TIME SPENT IN A GIVEN POLYNYA (bins) (all seals)
#=================================================================================
file_name = "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/DATA_COVERAGE_by_year_polynyas_bins.pdf"

bins = c("[0-5]", "]5-10]", "]10-15]", "]15-20]", "]20-25]", "]25-31]")
pal = c("#DEF5E5FF", "#60CEACFF", "#3497A9FF", "#395D9CFF", "#382A54FF", "#0B0405FF")

names(pal) = bins

summary_time_spent_perc_yr_month <- dives %>%
  filter(pol > 0) %>%
  mutate(day = day(DE_DATE)) %>%
  group_by(pol, month, year) %>%
  select(c(pol, month, year, day)) %>%
  distinct() %>%
  summarize(n_days = n()) %>%
  mutate(cat = case_when(n_days > 25 ~ "]25-31]",
                         n_days > 20 ~ "]20-25]",
                         n_days > 15 ~ "]15-20]",
                         n_days > 10 ~ "]10-15]",
                         n_days > 5 ~ "]5-10]",
                         n_days <= 5 ~ "[0-5]"))

summary_time_spent_perc_yr_month$cat <- factor(summary_time_spent_perc_yr_month$cat, levels = rev(bins))
labs <- c(rev(bins), "")

years <- sort(unique(summary_time_spent_perc_yr_month$year))

pdf(file_name,
    height = 6,
    width = 15)

for (yr in years) {
  
  summary <- summary_time_spent_perc_yr_month %>%
    filter(year == yr)
  
  all_pol_summary <- polynya_info %>% left_join(summary, by = c("ID" = "pol")) #___include all polynyas, even those w/o data
  
  month_df <- data_frame(month = rep(seq(1:12), 21), pol = rep(seq(1:21), each = 12))
  all_pol_summary_all_month <- all_pol_summary %>% 
    full_join(month_df, by = c("month", "ID" = "pol")) #___include all months, even those w/o data
  
  heatmap <- 
    ggplot(all_pol_summary_all_month, aes(x = factor(month), as.factor(ID))) + 
    scale_y_discrete(labels = polynya_info_dict) +
    scale_x_discrete(labels = seq(1:12), limits = factor(seq(1:12))) +
    xlab("Month") +
    ylab("") +
    labs(fill = "Number of days \nwith data") +
    ggtitle(yr) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 15, face="bold"),
          axis.text.x = element_text(size = 20),
          axis.title = element_text(size = 25, face="bold"),
          plot.title = element_text(size = 30, face = "bold"),
          legend.text = element_text(size = 20), #__________ legend texts
          legend.title = element_text(size = 20, face="bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "white")) 
  
  heatmap <- heatmap + 
    geom_tile(aes(fill = cat), col = "white") +
    scale_fill_manual(values = pal, na.value = NA, limits = rev(bins)) #___limits -> display all bins in colorscale
  
  print(heatmap)
}

dev.off()

## End script
rm(list=ls())

