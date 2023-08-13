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
## ---------------------------
## Paths
## ---------------------------
## Functions
source("~/Desktop/NASA-hotspots/palettes/palette_WM.R")
## ---------------------------

ctd <- readRDS("ctd_data/ctd_profiles_table")
stations <- readRDS("ctd_data/ctd_stations_table_north_bound_interp")

id_ctd_pol <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0) %>%
  select(id_ctd) %>%
  unique() %>%
  pull(id_ctd)

stations_pol <- stations %>%
  filter(id_ctd %in% id_ctd_pol)

# stations_pol <- readRDS("ctd_stations_table_polynyas") %>%
#   filter(pol > 0)

ctd_pol <- ctd %>%
  filter(id_ctd %in% stations_pol$id_ctd) #___keep only ctd stations assigned to dives in polynya

rm(ctd)
#==================================================================
# 1) SAMPLE CTD STATIONS (example)
#==================================================================

ctd_sample <- ctd %>%
  filter(REF == "ct109-939-14" & station == 190 | REF == "ct77-824-12" & station == 59)

stations_sub <- stations %>%
  select(c(REF, station, interpLat, interpLon))

ctd_sample <- ctd_sample %>% left_join(stations_sub, by = c("REF", "station"))

#------------------------------------------------------------------
# a- Compute surface freezing point temperature, pressure 
#    and potential temperature for CTD each observation
#------------------------------------------------------------------
ctd_sample <- ctd_sample %>%
  mutate(Tf = swTFreeze(psal, pressure = 0), #___Surface freezing point
         pressure = swPressure(depth, interpLat, eos = getOption("oceEOS", default="gsw")), #___pressure in dbar
         potTemp = swTheta(psal, temp, pressure, interpLon, interpLat,
                           referencePressure = 0, eos = getOption("oceEOS", default="gsw")))

## Plot temperature and potential temperature profiles for each station
ggplot() +
  geom_path(data = ctd_sample, aes(x = potTemp, y = -pressure, group = station), col = "red") +
  geom_path(data = ctd_sample, aes(x = temp, y = -pressure, group = station))

#--------------------------------------------------------------------------------
# b- Construct matrix for each variable (salinity, temperature, pressure, lon, lat)
#    => one column = one station
#--------------------------------------------------------------------------------
sal_matrix <- ctd_sample %>%
  select(c(id_ctd, psal, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, psal) %>%
  ungroup() %>%
  select(!depth)

temp_matrix <- ctd_sample %>%
  select(c(id_ctd, temp, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, temp) %>%
  ungroup() %>%
  select(!depth)

pressure_matrix <- ctd_sample %>%
  select(c(id_ctd, pressure, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, pressure) %>%
  ungroup() %>%
  select(!depth)
  
# lon <- ctd_sample %>%
#   select(id_ctd) %>%
#   distinct() %>%
#   left_join(stations, by = "id_ctd")

lonLat <- stations %>%
  select(c(id_ctd, interpLon, interpLat)) %>%
  filter(id_ctd %in% ctd_sample$id_ctd)

lat <- lonLat %>%
  pull(interpLat)

lon <- lonLat %>%
  pull(interpLon)

## Write CSV for each matrix
write.csv(sal_matrix, "test_gamma_n/salinity.csv", row.names = F)
write.csv(temp_matrix, "test_gamma_n/temp.csv", row.names = F)
write.csv(pressure_matrix, "test_gamma_n/pressure.csv", row.names = F)
write.csv(lat, "test_gamma_n/lat.csv", row.names = F)
write.csv(lon, "test_gamma_n/lon.csv", row.names = F)

#----------------------------------------------------------------------------------
# c- GO TO MATLAB => compute neutral density for each observation at each station)
#----------------------------------------------------------------------------------

#------------------------------------------------------------------
# d- ATTRIBUTE NEUTRAL DENSITY (gamma) TO CTD DATAFRAME
#------------------------------------------------------------------
gamma <- read.csv("test_gamma_n/gamma.csv", header = T, sep = ";", check.names = F)
ids <- colnames(gamma)
depths <- seq(1, nrow(gamma))
gamma$depth <- depths
gamma <- gamma %>%
  relocate(depth)

df_gather <- gamma %>%
  gather(key = depth) %>%
  rename(id_ctd = depth, gamma = value) %>%
  mutate(depth = rep(depths, 2)) %>%
  na.omit()

df_gather$id_ctd <- as.numeric(df_gather$id_ctd)

ctd_sample <- ctd_sample %>%
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

ctd_gamma <- ctd_pol %>%
  mutate(water_mass = case_when(psal < 34.4 & potTemp > Tf & gamma < 28 ~ 1,
                                potTemp > Tf + 0.1 & gamma > 28 & gamma < 28.27 ~ 2,
                                potTemp < Tf - 0.05 ~ 3,
                                psal > 34.5 & Tf - 0.05 < potTemp & Tf + 0.1 > potTemp & gamma > 28.27 ~ 4,
                                Tf + 0.1 < potTemp & potTemp < -1.7 & gamma > 28.2 ~ 5,
                                .default = NA))

write.csv(ctd_gamma, "test_gamma_n/ctd_profiles_table_WM.csv", row.names = F)


#------------------------------------------------------------------
# f- TS diagram
#------------------------------------------------------------------
ctd_ts <- as.ctd(salinity = ctd_gamma$psal, 
                 temperature = ctd_gamma$potTemp,
                 pressure = ctd_gamma$pressure,
                 longitude = ctd_gamma$interpLon,
                 latitude = ctd_gamma$interpLat)


lev_gamma <- seq(26, 27.6, 0.2)
cm <- colormap(ctd_gamma$water_mass, col = oceColorsViridis, breaks = c(1,2,3,4,5))
drawPalette(cm$zlim, col = cm$col, breaks = cm$breaks)
plotTS(with(ctd_gamma, ctd_ts), eos = "unesco", 
       pch = 19, cex = .8, col = cm$zcol, mar = par('mar'), 
       levels = lev_gamma)

## TS diagram
ggplot() +
  geom_point(data = ctd_gamma, aes(x = psal, y = potTemp, col = factor(water_mass)), size = .5) +
  theme_bw() +
  scale_color_viridis_d(na.value = "grey")


#==================================================================
# 2) ALL CTD STATIONS
#==================================================================
stations_sub <- stations_pol %>%
  select(c(REF, station, interpLat, interpLon))

ctd_pol <- ctd_pol %>% left_join(stations_sub, by = c("REF", "station"))

#------------------------------------------------------------------
# a- Compute surface freezing point temperature, pressure 
#    and potential temperature for CTD each observation
#------------------------------------------------------------------
ctd_pol <- ctd_pol %>%
  mutate(Tf = swTFreeze(psal, pressure = 0), #___surface freezing point
         pressure = swPressure(depth, interpLat, eos = getOption("oceEOS", default = "gsw")), #___pressure in dbar
         potTemp = swTheta(psal, temp, pressure, interpLon, interpLat,
                           referencePressure = 0, eos = getOption("oceEOS", default = "gsw"))) #___potential temperature in °C

#--------------------------------------------------------------------------------
# b- Construct matrix for each variable (salinity, temperature, pressure, lon, lat)
#    => one column = one station
#--------------------------------------------------------------------------------
sal_matrix <- ctd_pol %>%
  select(c(id_ctd, psal, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, psal) %>%
  ungroup() %>%
  select(!depth)

sal_matrix[is.na(sal_matrix)] = -999
colnames(sal_matrix) = paste0("Station_", colnames(sal_matrix))

temp_matrix <- ctd_pol %>%
  select(c(id_ctd, temp, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, temp) %>%
  ungroup() %>%
  select(!depth)

temp_matrix[is.na(temp_matrix)] = -999
colnames(temp_matrix) = paste0("Station_", colnames(temp_matrix))

pressure_matrix <- ctd_pol %>%
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

id_matrix <- ctd_pol %>%
  select(c(id_ctd, psal, depth)) %>%
  group_by(id_ctd) %>%
  spread(id_ctd, psal) %>%
  select(!depth)

## Write CSV for each matrix
write.csv(sal_matrix, "gamma_n_data/salinity.csv", row.names = F)
write.csv(temp_matrix, "gamma_n_data/temp.csv", row.names = F)
write.csv(pressure_matrix, "gamma_n_data/pressure.csv", row.names = F)
write.csv(lat, "gamma_n_data/lat.csv", row.names = F)
write.csv(lon, "gamma_n_data/lon.csv", row.names = F)

#---------------------------------------------------------------------------------
# c- GO TO MATLAB => compute neutral density for each observation at each station
#---------------------------------------------------------------------------------

#------------------------------------------------------------------
# d- ATTRIBUTE NEUTRAL DENSITY (gamma) TO CTD DATAFRAME
#------------------------------------------------------------------
nstations <- length(colnames(id_matrix))
gamma <- read.csv("gamma_n_data/gamma.csv", header = F, sep = ";", check.names = F)
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

ctd_pol <- ctd_pol %>%
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

ctd_gamma <- ctd_pol %>%
  mutate(water_mass = case_when(psal < 34.4 & potTemp > Tf & gamma < 28 ~ 1,
                                potTemp > Tf + 0.1 & gamma > 28 & gamma < 28.27 ~ 2,
                                potTemp < Tf - 0.05 ~ 3,
                                psal > 34.5 & Tf - 0.05 < potTemp & Tf + 0.1 > potTemp & gamma > 28.27 ~ 4,
                                Tf + 0.1 < potTemp & potTemp < -1.7 & gamma > 28.2 ~ 5,
                                .default = NA))

stations_pol <- stations_pol %>%
  mutate(month = month(time),
         season = case_when(month <= 2 ~ "Summer",
                            month >= 3 & month <= 5 ~ "Autumn",
                            month >= 6 & month <= 8 ~ "Winter",
                            .default = "Spring"))

saveRDS(stations_pol, "gamma_n_data/ctd_stations_table_polynyas_season")
saveRDS(ctd_gamma, "gamma_n_data/ctd_profiles_table_WM")

#------------------------------------------------------------------
# f- TS diagram
#------------------------------------------------------------------
ctd_gamma <- readRDS("gamma_n_data/ctd_profiles_table_WM")
stations_pol <- readRDS("gamma_n_data/ctd_stations_table_polynyas_season")
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
  
    stat_pol_unik <- stations_pol %>%
      filter(pol == id_pol)
    
    ctd_pol <- ctd_gamma %>%
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
