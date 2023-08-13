## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-26
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
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Heatmap time spent in each polynya

dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(!(pol %in% c(0, 1, 6, 7, 8, 18, 20))) %>%
  dplyr::select(c(REF, DE_DATE, pol)) %>%
  mutate(yday = yday(DE_DATE), year = year(DE_DATE)) %>%
  arrange_at("year")

toto <- dives %>%
  filter(REF == "ct3_9926_04")

hist(as.numeric(diff(toto$DE_DATE))/3600, breaks = 1000)

summary(as.numeric(diff(toto$DE_DATE))/60)

sum_dives <- dives %>%
  group_by(REF, pol, yday, year) %>%
  count() %>%
  arrange_at("year") %>%
  filter(yday < 262)

ids_pol <- sort(unique(sum_dives$pol))

for (id in ids_pol) {
  sum_dives_pol <- sum_dives %>%
    filter(pol == id) %>%
    arrange_at("year")# %>%
    #mutate(day = yday, origin = "2016-01-01", format = "%m-%d"))

  ref_year <- sum_dives_pol %>%
    ungroup() %>%
    dplyr::select(c(REF, year)) %>%
    distinct() %>%
    mutate(newID = paste0(year, "_", row_number())) %>%
    dplyr::select(c(REF, newID))
  
  sum_dives_pol <- sum_dives_pol %>% left_join(ref_year, by = "REF")
  
  ref_sorted <- unique(sum_dives_pol$newID)
  
  sum_dives_pol$newID <- factor(sum_dives_pol$newID, levels = rev(ref_sorted))
  
  
  nref <- sum_dives_pol %>%
    ungroup() %>%
    dplyr::select(c(REF)) %>%
    distinct() %>%
    count() %>%
    pull(n)
  
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/polynya_usage/heatmap_time_spent_pol/%s_test.png", id), 
      height = nref*1.2, width = 35, units = "cm", res = 150)
  
  heatmap_time_pol <-
    ggplot(sum_dives_pol, aes(yday, newID)) + 
    geom_tile(aes(fill = n)) +
    scale_fill_viridis_c() +
    #scale_y_discrete(labels = polynya_info_dict) +
    #scale_x_discrete(limits = c("01/01", "01/09")) +
    scale_x_continuous(limits = c(0, 262), breaks = seq(0, 365, by = 30)) +
    xlab("Day of the year") +
    ylab("") +
    #facet_grid(pol~.) +
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
  
  print(heatmap_time_pol)
  
  dev.off()
}

# ggsave(plot = heatmap_time_pol, "~/Dropbox/data/outputs_Marthe_2023/polynya_usage/heatmap_time_spent_polynyas.png", height = 150, width = 30, units = c("cm"), dpi = 300, limitsize = F)

## End script
rm(list=ls())
