## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-12
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
library(oce)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/palettes/palette_WM.R")
source("~/Desktop/NASA-hotspots/isoneutrals.R")
## ---------------------------

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

## CTD stations
stations_pol <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
  filter(id_ctd %in% id_ctd_pol$id_ctd)

ctd_pol <- readRDS("ctd_data/ctd_profiles_table") %>%
  select(c(id_ctd, depth, psal, temp)) %>%
  filter(id_ctd %in% stations_pol$id_ctd)

## Add polynya id
ctd_pol <- ctd_pol %>%
  left_join(id_ctd_pol, by = "id_ctd")

## Add time and position to ctd profiles for TS diagram
time_df <- stations_pol %>%
  select(c(interpLat, interpLon, id_ctd, time))

ctd_pol_time <- ctd_pol %>%
  left_join(time_df, by = "id_ctd") %>%
  mutate(year = year(time),
         month = month.name[month(time)])

rm(ctd_pol)
#------------------------------------------------------------------
# a- Compute surface freezing point temperature, pressure and potential temperature
#------------------------------------------------------------------
ctd_pol_time <- ctd_pol_time %>%
  mutate(Tf = swTFreeze(psal, pressure = 0), #___surface freezing point from practical salinity
         pressure = swPressure(depth, interpLat, eos = getOption("oceEOS", default = "gsw")), #___pressure in dbar
         potTemp = swTheta(psal, temp, pressure, interpLon, interpLat,
                           referencePressure = 0, eos = getOption("oceEOS", default = "gsw"))) #___potential temperature in °C

saveRDS(ctd_pol_time, "ctd_data/ctd_profiles_table_Tf_P_theta")

#------------------------------------------------------------------
# b- TS diagram
#------------------------------------------------------------------

## Isoneutrals mCDW
iso28 = isoneutrals(28)
iso28.27 = isoneutrals(28.27)

for (id_pol in selection) {
  data = ctd_pol_time %>%
    filter(pol == id_pol)
  
  ## Surface freezing temperature line
  psal = seq(min(data$psal), max(data$psal), 0.1)
  sft = tibble(SP = psal, t = swTFreeze(psal, 0))
  
  data$month <- factor(data$month, levels = month.name)
  data <- data %>%
    arrange_at("month")
  
  ## TS plot with time
  TS_tplot <- ggplot() +
    geom_point(data = data, aes(x = psal, y = potTemp, col = month), size = .2) +
    geom_line(data = iso28, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = iso28.27, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = sft, aes(x = SP, y = t), lty = 2, col = "black") +
    scale_color_viridis_d("Month", option = "G", ) +
    #facet_wrap(~year, ncol = 1, scales = "free") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/month/pol_%s_time_TS.png", id_pol),
      height = 15, width = 17, units = "cm", res = 300)
  print(TS_tplot)
  dev.off()
}

#--------------------------------------------------------------------------------
# c- Construct matrix for each variable
#    => one column = one station
#--------------------------------------------------------------------------------
ctd_pol_time <- readRDS("ctd_data/ctd_profiles_table_Tf_P_theta") %>%
  distinct()

psal <- ctd_pol_time %>%
  select(c(id_ctd, psal, depth)) %>%
  distinct()

## TODO: build one object with all data for .mat
ids <- unique(ctd_pol_time$id_ctd)
psal_matrix <- psal %>%
  pivot_wider(names_from = id_ctd, values_from = psal) %>%
  select(!depth)

psal_matrix[is.na(psal_matrix)] = -999
colnames(psal_matrix) = paste0("Station_", colnames(psal_matrix))

t_matrix <- ctd_pol_time %>%
  select(c(id_ctd, temp, depth)) %>%
  distinct() %>%
  pivot_wider(names_from = id_ctd, values_from = temp) %>%
  select(!depth)

t_matrix[is.na(t_matrix)] = -999
colnames(t_matrix) = paste0("Station_", colnames(t_matrix))

pressure_matrix <- ctd_pol_time %>%
  select(c(id_ctd, pressure, depth)) %>%
  distinct() %>%
  pivot_wider(names_from = id_ctd, values_from = pressure) %>%
  select(!depth)

pressure_matrix[is.na(pressure_matrix)] = -999
colnames(pressure_matrix) = paste0("Station_", colnames(pressure_matrix))

lonLat <- stations_pol %>%
  select(c(id_ctd, interpLon, interpLat)) %>%
  filter(id_ctd %in% ctd_pol_time$id_ctd)

lat <- lonLat %>%
  pull(interpLat)

lon <- lonLat %>%
  pull(interpLon)

id_matrix <- ctd_pol_time %>%
  select(c(id_ctd, psal, depth)) %>%
  distinct() %>%
  pivot_wider(names_from = id_ctd, values_from = psal) %>%
  select(c(!depth))


## Write CSV for each matrix
write.csv(psal_matrix, "ctd_data/gamma_n_data/psalinity.csv", row.names = F)
write.csv(t_matrix, "ctd_data/gamma_n_data/temp.csv", row.names = F)
write.csv(pressure_matrix, "ctd_data/gamma_n_data/pressure.csv", row.names = F)
write.csv(lat, "ctd_data/gamma_n_data/lat.csv", row.names = F)
write.csv(lon, "ctd_data/gamma_n_data/lon.csv", row.names = F)

#-------------------------------------------------------------------------------------------
# d- GO TO MATLAB => compute neutral density for each observation at each station (EOS 80)
#-------------------------------------------------------------------------------------------
library(R.matlab)
system("matlab -nodisplay -r \"run('~/Desktop/NASA-hotspots/eos80_legacy_gamma_n/run_eos80_gamma_n.m'); exit\"")

#------------------------------------------------------------------
# e- ATTRIBUTE NEUTRAL DENSITY (gamma) TO CTD DATAFRAME
#------------------------------------------------------------------
nstations <- length(colnames(id_matrix))
gamma <- read.csv("ctd_data/gamma_n_data/gamma.csv", header = F, sep = ";", check.names = F)
colnames(gamma) <- colnames(id_matrix)
depths <- seq(1, nrow(gamma))
gamma$depth <- depths
gamma <- gamma %>%
  relocate(depth)

df_gather <- gamma %>%
  gather(key = depth) %>%
  rename(id_ctd = depth, gamma = value) %>%
  mutate(depth = rep(depths, nstations)) %>%
  na.omit()

df_gather$id_ctd <- as.numeric(df_gather$id_ctd)

ctd_pol_time <- ctd_pol_time %>%
  left_join(df_gather, by = c("id_ctd", "depth"))

#------------------------------------------------------------------
# f- DEFINE WATER MASSES
#------------------------------------------------------------------

## Criteria: 
# 1 = AASW: S < 34.4 & 𝜃 > Tf & 𝛾 < 28
# 2 = mCDW: 𝜃 > Tf + 0.1 & 28 < 𝛾 < 28.27
# 3 = ISW: 𝜃 < Tf − 0.05
# 4 = DSW: S > 34.5 & Tf − 0.05 < 𝜃 < Tf + 0.1 & 𝛾 > 28.27
# 5 = mSW: Tf + 0.1 < 𝜃 < - 1.7 & 𝛾 > 28.2
# 6 = LSSW

## Criteria: 
# 1 = AASW: 𝜃 > Tf & 𝛾 < 28
# 2 = mCDW: 𝜃 > Tf & 28 < 𝛾 < 28.27
# 3 = ISW: 𝜃 < Tf
# 4 = DSW: Tf < 𝜃 < Tf + 0.1 & 𝛾 > 28.27
# 5 = mSW: Tf + 0.1 < 𝜃 < 1.7 & 𝛾 > 28.2

ctd_gamma <- ctd_pol_time %>%
  mutate(water_mass = case_when(potTemp > Tf - 0.5 & gamma < 28 ~ "AASW",
                                potTemp > Tf + 0.1 & between(gamma, 28, 28.27) ~ "mCDW",
                                potTemp < Tf - 0.05 ~ "ISW",
                                between(potTemp, Tf - 0.05, Tf + 0.1) & gamma > 28.27 ~ "DSW",
                                between(potTemp, Tf + 0.1, 1.7) & gamma > 28.2 ~ "mSW",
                                between(potTemp, Tf - 0.05, Tf + 0.1) & between(gamma, 28, 28.27) ~ "LSSW",
                                .default = "other"))

ctd_gamma <- ctd_pol_time %>%
  mutate(water_mass = case_when(potTemp > Tf & gamma < 28 ~ "AASW",
                                potTemp > Tf & between(gamma, 28, 28.27) ~ "mCDW",
                                potTemp < Tf ~ "ISW",
                                between(potTemp, Tf, Tf + 0.1) & gamma > 28.27 ~ "DSW",
                                between(potTemp, Tf + 0.1, 1.7) & gamma > 28.2 ~ "mSW",
                                .default = "other"))

stations_pol <- stations_pol %>%
  mutate(month = month(time),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring"))

saveRDS(stations_pol, "ctd_data/gamma_n_data/ctd_stations_table_polynyas_season")
saveRDS(ctd_gamma, "ctd_data/gamma_n_data/ctd_profiles_table_WM")

## End script
rm(list=ls())
