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

stations <- readRDS("ctd_data/ctd_stations_table_north_bound_interp")

##### ONLY SHACKELTON FOR COMPARISON TEST WITH PORTELA 2022
id_ctd_pol <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol %in% selection) %>%
  mutate(month = month.name[month(DE_DATE)]) %>%
  filter(!month %in% c("September", "October", "November", "December")) %>% #___Jan-August
  select(c(id_ctd, pol)) %>%
  unique() %>%
  na.omit() #%>%

stations_pol <- stations %>%
  filter(id_ctd %in% id_ctd_pol$id_ctd)

ctd_pol <- readRDS("ctd_data/ctd_profiles_table") %>%
  filter(id_ctd %in% stations_pol$id_ctd) #___keep only ctd stations assigned to dives in polynya

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

#==================================================================
# 1) ALL CTD STATIONS
#==================================================================
#------------------------------------------------------------------
# a- Compute surface freezing point temperature, pressure and potential temperature
#------------------------------------------------------------------
ctd_pol_time <- ctd_pol_time %>%
  mutate(Tf = swTFreeze(psal, pressure = 0), #___surface freezing point from practical salinity
         pressure = swPressure(depth, interpLat, eos = getOption("oceEOS", default = "gsw")), #___pressure in dbar
         potTemp = swTheta(psal, temp, pressure, interpLon, interpLat,
                           referencePressure = 0, eos = getOption("oceEOS", default = "gsw"))) #___potential temperature in °C

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
  
  ## TS plot with time
  TS_tplot <- ggplot() +
    geom_point(data = data, aes(x = psal, y = potTemp, col = month), size = .2) +
    geom_line(data = iso28, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = iso28.27, aes(x = SP, y = t), lty = 2, col = "black") +
    geom_line(data = sft, aes(x = SP, y = t), lty = 2, col = "black") +
    #scale_y_continuous(limits = c(-2.5, 1)) +
    #scale_x_continuous(limits = c(32.5, 35)) +
    scale_color_viridis_d("Month", option = "G") +
    facet_wrap(~year, ncol = 1, scales = "free") +
    theme_bw() +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  png(sprintf("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/pol_%s_time_TS.png", id_pol),
      height = 60, width = 15, units = "cm", res = 300)
  print(TS_tplot)
  dev.off()
}

## Track
track_tplot <- ggplot() +
  geom_point(data = ctd_pol_time, aes(x = interpLon, y = interpLat, col = month), size = 1) +
  geom_path(data = ctd_pol_time, aes(x = interpLon, y = interpLat, col = month, group = REF), linewidth = .3) +
  facet_wrap(~year, ncol = 1, scales = "free") +
  scale_color_viridis_d("Month", option = "G") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 5)))

png("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/shackleton_time_track.png",
    height = 60, width = 15, units = "cm", res = 300)
print(track_tplot)
dev.off()

#--------------------------------------------------------------------------------
# b- Construct matrix for each variable
#    => one column = one station
#--------------------------------------------------------------------------------
psal_matrix <- ctd_pol_time %>%
  select(c(id_ctd, psal, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, psal) %>%
  ungroup() %>%
  select(!depth)

psal_matrix[is.na(psal_matrix)] = -999
colnames(psal_matrix) = paste0("Station_", colnames(psal_matrix))

thetat_matrix <- ctd_pol_time %>%
  select(c(id_ctd, temp, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, temp) %>%
  ungroup() %>%
  select(!depth)

thetat_matrix[is.na(thetat_matrix)] = -999
colnames(thetat_matrix) = paste0("Station_", colnames(thetat_matrix))

pressure_matrix <- ctd_pol_time %>%
  select(c(id_ctd, pressure, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, pressure) %>%
  ungroup() %>%
  select(!depth)

pressure_matrix[is.na(pressure_matrix)] = -999
colnames(pressure_matrix) = paste0("Station_", colnames(pressure_matrix))

# lon <- ctd_sample %>%
#   select(id_ctd) %>%
#   distinct() %>%
#   left_join(stations, by = "id_ctd")

lonLat <- stations_pol %>%
  select(c(id_ctd, interpLon, interpLat)) %>%
  filter(id_ctd %in% ctd_pol$id_ctd)

lat <- lonLat %>%
  pull(interpLat)

lon <- lonLat %>%
  pull(interpLon)

id_matrix <- ctd_pol_time %>%
  select(c(id_ctd, psal, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, psal) %>%
  select(!depth)

## Write CSV for each matrix
write.csv(psal_matrix, "ctd_data/gamma_n_data/psalinity.csv", row.names = F)
write.csv(thetat_matrix, "ctd_data/gamma_n_data/pot_temp.csv", row.names = F)
write.csv(pressure_matrix, "ctd_data/gamma_n_data/pressure.csv", row.names = F)
write.csv(lat, "ctd_data/gamma_n_data/lat.csv", row.names = F)
write.csv(lon, "ctd_data/gamma_n_data/lon.csv", row.names = F)

#-------------------------------------------------------------------------------------------
# c- GO TO MATLAB => compute neutral density for each observation at each station (EOS 80)
#-------------------------------------------------------------------------------------------

#------------------------------------------------------------------
# d- ATTRIBUTE NEUTRAL DENSITY (gamma) TO CTD DATAFRAME
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
# e- DEFINE WATER MASSES
#------------------------------------------------------------------

## Criteria: 
# 1 = AASW: S < 34.4 & 𝜃 > Tf & 𝛾 < 28
# 2 = mCDW: 𝜃 > Tf + 0.1 & 28 < 𝛾 < 28.27
# 3 = ISW: 𝜃 < Tf − 0.05
# 4 = DSW: S > 34.5 & Tf − 0.05 < 𝜃 < Tf + 0.1 & 𝛾 > 28.27
# 5 = mSW: Tf + 0.1 < 𝜃 < - 1.7 & 𝛾 > 28.2
# 6 = LSSW

ctd_gamma <- ctd_pol_time %>%
  mutate(water_mass = case_when(psal < 34.4 & potTemp > Tf & gamma < 28 ~ 1,
                                potTemp > Tf + 0.1 & gamma > 28 & gamma < 28.27 ~ 2,
                                potTemp < Tf - 0.05 ~ 3,
                                psal > 34.5 & Tf - 0.05 < potTemp & Tf + 0.1 > potTemp & gamma > 28.27 ~ 4,
                                Tf + 0.1 < potTemp & potTemp < -1.7 & gamma > 28.2 ~ 5,
                                .default = 6))

ctd_gamma <- ctd_pol_time %>%
  mutate(water_mass = case_when(potTemp > Tf - 0.05 & gamma < 28 ~ 1,
                                potTemp > Tf + 0.1 & gamma > 28 & gamma < 28.27 ~ 2,
                                potTemp < Tf - 0.05 ~ 3,
                                Tf - 0.05 < potTemp & Tf + 0.1 > potTemp & gamma > 28.27 ~ 4,
                                Tf + 0.1 < potTemp & potTemp < 1.7 & gamma > 28.2 ~ 5,
                                potTemp < Tf + 0.1 & potTemp > Tf - 0.05 & gamma > 28 & gamma < 28.27 ~ 6,
                                .default = NA))

stations_pol <- stations_pol %>%
  mutate(month = month(time),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring"))

saveRDS(stations_pol, "ctd_data/gamma_n_data/ctd_stations_table_polynyas_season")
saveRDS(ctd_gamma, "ctd_data/gamma_n_data/ctd_profiles_table_WM")

## TS plot with WM
TS_wmplot <- ggplot() +
  geom_point(data = ctd_gamma, aes(x = psal, y = potTemp, col = factor(water_mass)), size = .05) +
  geom_line(data = iso28, aes(x = SP, y = t), lty = 2, col = "black") +
  geom_line(data = iso28.27, aes(x = SP, y = t), lty = 2, col = "black") +
  geom_line(data = sft, aes(x = SP, y = t), lty = 2, col = "black") +
  #scale_y_continuous(limits = c(-2.5, 1)) +
  #scale_x_continuous(limits = c(32.5, 35)) +
  scale_color_viridis_d("Month") +
  facet_wrap(~year, ncol = 1, scales = "free") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 5)))

png("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/shackleton_WM_TS.png",
    height = 60, width = 15, units = "cm", res = 300)
print(TS_wmplot)
dev.off()


#------------------------------------------------------------------
# diagram TS Shackleton
#------------------------------------------------------------------

# TODO: save table with depth, psal, potential temp, WM
stations_pol_time <- stations_pol %>%
  select(c(id_ctd, time))

ctd_gamma_time <- ctd_gamma %>%
  left_join(stations_pol_time, by = "id_ctd") %>%
  mutate(year = year(time))

toto <- ctd_gamma_time %>%
  select(c(psal, potTemp, water_mass, time, year))
write.csv(toto, "~/Desktop/test.csv")

library(lubridate)
sal_p <- ggplot() +
  geom_point(data = ctd_gamma_time, aes(x = time, y = -depth, col = psal), size = .1) +
  #scale_color_manual(values = palette_WM) +
  facet_wrap(factor(year)~., scales = "free_x", ncol = 1) +
  theme_minimal()

png("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/shackleton_timeline_salinity.png",
    height = 30, width = 20, units = "cm", res = 150)
print(sal_p)
dev.off()

wm_p <- ggplot() +
  geom_point(data = ctd_gamma_time, aes(x = time, y = -depth, col = factor(water_mass)), size = .1) +
  #scale_color_manual(values = palette_WM) +
  facet_wrap(factor(year)~., scales = "free_x", ncol = 1) +
  theme_minimal()

png("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/shackleton_timeline_wm.png",
    height = 30, width = 20, units = "cm", res = 150)
print(wm_p)
dev.off()

## 2011 (compare with Portela)


ctd_gamma_time <- ctd_gamma %>%
  left_join(stations_pol_time, by = "id_ctd") %>%
  mutate(year = year(time)) %>%
  filter(year == 2011)

ctd_ts <- as.ctd(salinity = ctd_gamma_time$psal, 
                 temperature = ctd_gamma_time$temp,
                 pressure = ctd_gamma_time$pressure,
                 longitude = ctd_gamma_time$interpLon,
                 latitude = ctd_gamma_time$interpLat)

col <- ctd_gamma_time %>%
  mutate(col = case_when(water_mass == 1 ~ "#ED7A5F",
                         water_mass == 2 ~ "#80CAAC",
                         water_mass == 3 ~ "#F4BC44",
                         water_mass == 4 ~ "#365C8DFF",
                         water_mass == 5 ~ "#23ADE4",
                         .default = "#E5E5E5")) %>%
  pull(col)

ctd_gamma_time <- ctd_gamma_time %>%
  mutate(sigma = swSigma(psal, temp, pressure, 
                         longitude = interpLon, latitude = interpLat, 
                         getOption("oceEOS", default = "gsw")))
# min_sig <- floor(min(ctd_pol_s$sigma, na.rm = T))
# max_sig <- floor(max(ctd_pol_s$sigma, na.rm = T))
# lev_gamma <- seq(min_sig, max_sig, length.out = 5)
min_psal <- min(ctd_gamma_time$psal, na.rm = T)
max_psal <- max(ctd_gamma_time$psal, na.rm = T)
min_theta <- min(ctd_gamma_time$potTemp, na.rm = T)
max_theta <- max(ctd_gamma_time$potTemp, na.rm = T)

print(plotTS(with(ctd_gamma_time, ctd_ts), eos = "gsw", 
             pch = 19, cex = .4, col = col, mar = par('mar'), 
             nlevels = 10,
             inSitu = T, ylab = "Potential temperature [°C]",
             Slim = c(min_psal - dS, max_psal + dS),
             Tlim = c(min_theta - dT, max_theta + dT)))
print(lines(isopycnals$psal, isopycnals$potTemp))
print(legend('topleft', legend = names(palette_WM), pch=19, col=palette_WM))
print()
print(title(s))

# ctd_winter <- stations_pol %>%
#   filter(season == "Autumn")
# 
# ctd_gamma_winter <- ctd_gamma %>%
#   filter(id_ctd %in% ctd_winter$id_ctd)
#   
# library(plotly)
# p <- plot_ly(width = 1000, height = 1000) %>% 
#   add_markers(data = ctd_gamma_winter, x = ~interpLon, y = ~interpLat, z = ~-depth, color = ~factor(water_mass),
#               marker = list(size = 2))
# p

#------------------------------------------------------------------
# f- TS diagram
#------------------------------------------------------------------
ctd_gamma <- readRDS("ctd_data/gamma_n_data/ctd_profiles_table_WM")
stations_pol <- readRDS("ctd_data/gamma_n_data/ctd_stations_table_polynyas_season")

## TODO: add pol id to ctd stations for TS diag
id_pols <- stations_pol %>%
  select(pol) %>%
  unique() %>%
  arrange(pol) %>%
  pull(pol)
seasons <- c("Summer", "Autumn", "Winter", "Spring")

dS = 0.1
dT = 0.1

for (id_pol in id_pols) {
  # pdf(sprintf("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/pol_%s_diag_TS.pdf", id_pol),
  #     height = 3, width = 5, pointsize = 5)
  
    stat_pol_unik <- stations_pol #%>%
      filter(pol == id_pol)
    
    ctd_pol <- ctd_gamma #%>%
      filter(id_ctd %in% stat_pol_unik$id_ctd)
    
    min_psal <- min(ctd_pol$psal, na.rm = T)
    max_psal <- max(ctd_pol$psal, na.rm = T)
    min_theta <- min(ctd_pol$potTemp, na.rm = T)
    max_theta <- max(ctd_pol$potTemp, na.rm = T)
    
    for (s in seasons) {
      png(sprintf("~/Dropbox/data/outputs_Marthe_2023/diagram_TS/png/pol_%s_%s_diag_TS.png", id_pol, s),
          height = 13, width = 20, units = "cm", res = 150)
      
      stat_pol_unik_season <- stat_pol_unik %>%
        filter(season == s)
      
      ctd_pol_s <- ctd_pol%>%
        filter(id_ctd %in% stat_pol_unik_season$id_ctd)
      
      if (nrow(ctd_pol_s) > 0) {
        ctd_ts <- as.ctd(salinity = ctd_pol_s$psal, 
                         temperature = ctd_pol_s$potTemp,
                         pressure = ctd_pol_s$pressure,
                         longitude = ctd_pol_s$interpLon,
                         latitude = ctd_pol_s$interpLat)
        
        col <- ctd_pol_s %>%
          mutate(col = case_when(water_mass == 1 ~ "#ED7A5F",
                                 water_mass == 2 ~ "#80CAAC",
                                 water_mass == 3 ~ "#F4BC44",
                                 water_mass == 4 ~ "#365C8DFF",
                                 water_mass == 5 ~ "#23ADE4",
                                 .default = "#E5E5E5")) %>%
          pull(col)
        
        ctd_pol_s <- ctd_pol_s %>%
          mutate(sigma = swSigma(psal, potTemp, pressure, 
                                 longitude = interpLon, latitude = interpLat, 
                         getOption("oceEOS", default = "gsw")))
        # min_sig <- floor(min(ctd_pol_s$sigma, na.rm = T))
        # max_sig <- floor(max(ctd_pol_s$sigma, na.rm = T))
        # lev_gamma <- seq(min_sig, max_sig, length.out = 5)
        print(plotTS(with(ctd_pol_s, ctd_ts), eos = "gsw", 
               pch = 19, cex = .4, col = col, mar = par('mar'), 
               nlevels = 10,
               inSitu = T, ylab = "Potential temperature [°C]",
               Slim = c(min_psal - dS, max_psal + dS),
               Tlim = c(min_theta - dT, max_theta + dT)))
        print(legend('topleft', legend = names(palette_WM), pch=19, col=palette_WM))
        print(title(s))
      }
      dev.off()
    }
  # dev.off()
}

## End script
rm(list=ls())
