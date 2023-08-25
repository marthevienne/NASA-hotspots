## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-18
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
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/isoneutrals.R")
## ---------------------------

## Selected polynyas
source("~/Desktop/NASA-hotspots/selected_polynyas_analysis.R")

## CTD ids in polynyas between January-August
id_ctd_pol <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol %in% selection) %>%
  mutate(month = month.name[month(DE_DATE)],
         year = year(DE_DATE)) %>%
  filter(month %in% month.name[1:8] & year <= 2021) %>% #___Jan-August and year 2022 removed
  dplyr::select(c(id_ctd, pol)) %>%
  distinct() %>%
  na.omit() #%>%

## CTD with neutral density
ctd_gamma <- readRDS("ctd_data/gamma_n_data/ctd_profiles_table_WM")

## Foraging segments
fs <- readRDS("behavioural_data/hunting_time_segments")

## Select only CTD from Jan-August and before 2022 (data not corrected for now)
id_ctd_selected <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol %in% selection) %>% #___only in selected polynyas
  mutate(year = year(DE_DATE),
         month = month(DE_DATE)) %>%
  filter(year <= 2021 & month <= 8) %>%
  dplyr::select(c(REF, NUM, id_ctd))

## Add REF and NUM to join fs and ctd_gamma
ctd_gamma_ref_d <- ctd_gamma %>%
  left_join(id_ctd_selected, by = "id_ctd") %>%
  dplyr::select(c(REF, NUM, id_ctd, psal, potTemp, depth))

fs_TS <- fs %>%
  left_join(ctd_gamma_ref_d, by = c("REF", "NUM")) %>%
  na.omit() %>%
  filter(depth == mean_depth_HS)

## Add polynya id
ctd_gamma <- ctd_gamma %>%
  left_join(id_ctd_pol, by = "id_ctd")

## Add polynya id
fs_TS <- fs_TS %>%
  left_join(id_ctd_pol, by = "id_ctd")

## Isoneutrals
iso28 <- isoneutrals(28)
iso28.27 <- isoneutrals(28.27)

## TS plot with WM
for (id_pol in selection) {
  data = ctd_gamma %>%
    filter(pol == id_pol)
  
  data_fs = fs_TS %>%
    filter(pol == id_pol)
  
  ## Surface freezing temperature line
  psal = seq(min(data$psal), max(data$psal), 0.1)
  sft = tibble(SP = psal, t = swTFreeze(psal, 0))
  
  ## TS plot with time
  TS_wmplot <- ggplot() +
    geom_point(data = data, aes(x = psal, y = potTemp, col = water_mass), size = .2) +
    geom_point(data = data_fs, aes(x = psal, y = potTemp), size = .1, shape = 16) +
    geom_line(data = iso28, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = iso28.27, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = sft, aes(x = SP, y = t), lty = 2, col = "black") +
    scale_color_manual("Water mass", values = palette_WM) +
    #facet_wrap(~year, ncol = 1, scales = "free") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/water_mass/pol_%s_WM_FS_TS.png", id_pol),
      height = 15, width = 17, units = "cm", res = 300)
  print(TS_wmplot)
  dev.off()
}

## End script
rm(list=ls())


