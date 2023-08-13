## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-17
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
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Dives
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0)

## CTD stations in polynya
stations <- readRDS("ctd_data/ctd_stations_table_north_bound_interp")

stations_pol <- stations %>%
  filter(id_ctd %in% dives$id_ctd)

lonLat <- stations_pol %>%
  select(c(id_ctd, interpLon, interpLat))

## CTD profiles
ctd <- readRDS("ctd_data/ctd_profiles_table") %>%
  filter(id_ctd %in% stations_pol$id_ctd) %>%
  left_join(lonLat, by = "id_ctd")

#------------------------------------------------------------------
# ALL STATIONS
#------------------------------------------------------------------

## Compute pressure
ctd <- ctd %>%
  mutate(pressure = swPressure(depth, interpLat, eos = getOption("oceEOS", default = "gsw")))

## Calculate dsigma = sigma_prof - sigma_10m
ctd <- ctd %>%
  mutate(sigmaTheta = swSigmaTheta(salinity = psal, 
                                   temperature = temp, 
                                   pressure = pressure, 
                                   longitude = interpLon, 
                                   latitude = interpLat, 
                         eos = getOption("oceEOS", default = "gsw")))

ggplot() +
  geom_path(data = ctd[1:2000,], aes(x = sigmaTheta, y = -pressure, group = id_ctd)) +
  theme_minimal()

ctd_dSigmaT <- ctd %>%
  group_by(id_ctd) %>%
  mutate(dSigmaT = sigmaTheta - nth(sigmaTheta, 10)) 

ggplot() +
  geom_path(data = ctd_dSigmaT[1:2000,], aes(x = dSigmaT, y = -pressure, group = id_ctd)) +
  theme_minimal() +
  scale_y_continuous(limits = c(- 50, 0), n.breaks = 10) +
  geom_vline(xintercept = 0.03)


ctd_MLD <- ctd_dSigmaT %>%
  filter(dSigmaT >= 0.03) %>%
  group_by(id_ctd) %>%
  arrange(depth, .by_group = T) %>%
  filter(row_number() == 1) %>%
  select(c(id_ctd, MLD = depth, dSigmaT))

summary(ctd_MLD$MLD)

stations_MLD <- stations %>%
  left_join(ctd_MLD, by = "id_ctd")

ggplot(stations_MLD, aes(x = time, fill = id_ctd, col = id_ctd)) + 
  geom_bar(aes(y = -MLD), stat = "identity", position = "dodge")

plot(stations_MLD$time, -stations_MLD$MLD, type = "p", pch = 16, cex = .4)

saveRDS(stations_MLD, "ctd_data/ctd_stations_table_north_bound_interp_MLD")

#------------------------------------------------------------------
# TEST ON 2 STATIONS
#------------------------------------------------------------------

stat_sp <- stations_pol %>%
  filter(REF == "ft07-Cy29-11" & station == 102 | REF == "ft07-Cy29-11" & station == 56)

ctd_sp <- ctd %>%
  filter(REF == "ft07-Cy29-11" & station == 102 | REF == "ft07-Cy29-11" & station == 56)

## Calculate dsigma = sigma_prof - sigma_10m
ctd_sp <- ctd_sp %>%
  mutate(sigma = swSigma(psal, temp, pressure, longitude = interpLon, latitude = interpLat, 
                         getOption("oceEOS", default = "gsw")))

ggplot() +
  geom_path(data = ctd_sp, aes(x = sigma, y = -pressure, group = id_ctd)) +
  theme_minimal()

ctd_sp <- ctd_sp %>%
  group_by(id_ctd) %>%
  mutate(dsigma = sigma - nth(sigma, 10)) %>%
  filter(dsigma >= 0.03) %>%
  group_by(id_ctd) %>%
  arrange(depth, .by_group = T) %>%
  filter(row_number() == 1)

## End script
rm(list=ls())
