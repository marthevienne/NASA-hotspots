## ---------------------------
##
## Script name: compute_ctd_tables
##
## Purpose of script: 1) List of CTD file paths only for selected seals
##                    2) Construct CTD stations table and CTD profiles table (compiled)
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-07-03
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes: Error on netcdf ADJUSTED variables:  Contains the error on the adjusted values as determined by the delayed mode QC process
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
## ---------------------------
## Library
library(dplyr)
library(tidyr)
library(stringr)
library(ncdf4)
## ---------------------------
## Paths
path_ctd <- "~/Dropbox/data/data_oceanographic/"
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/functions_raster/netcdf_to_raster.R")
source("~/Desktop/NASA-hotspots/useful_functions/string_to_array.R")
source("~/Desktop/NASA-hotspots/useful_functions/list_to_array.R")
## ---------------------------

#==================================================================
# 1) LIST OF CTD PATH FILES OF SELECTED SEALS
#==================================================================

## Retrieve seals REF in dive data
seals <- read.csv("~/Desktop/WHOI/Data/output_data/seals.csv") %>%
  filter(n_dives_visit_polynya > 0)

seals$REF <- gsub("_", "-", seals$REF)

## Retrieve deployment numbers
deploy <- seals %>%
  pull(REF) %>%
  str_extract("[^-]+") %>%
  unique()

## For each seal in each deployment, get ctd file path by order of resolution (hr2 > hr1)
df_files <- data_frame(REF = seals$REF, ctd_file = NA)

# type = "ODV"
type = "nc"

for (d in deploy) {
  regex <- paste0(d, "-")
  ref_d <- seals %>%
    filter(row_number() %in% grep(regex, seals$REF)) %>%
    pull(REF)
  
  ref_d <- gsub("_", "-", ref_d)
  
  path <- paste0(path_ctd, d, "/")
  
  for (ref in ref_d) {
    if (type == "nc") {
      file <- list.files(path,
                         pattern = paste0(ref, '_hr2_prof.nc'),
                         recursive = T,
                         full.names = T)
      if (identical(file, character(0))) {
        file <- list.files(path,
                           pattern = paste0(ref, '_hr1_prof.nc'),
                           recursive = T,
                           full.names = T)
        
      }
    } else if (type == "ODV") {
      file <- list.files(path,
                         pattern = paste0(ref, '_ODV.txt'),
                         recursive = T,
                         full.names = T)
      
    }
    
    if (identical(file, character(0))) {
      file <- NA
    }
    
    df_files$ctd_file[df_files$REF == ref] <- file
  }
}
  
## Investigate NA files
files_NA <- df_files %>%
  filter(is.na(ctd_file)) %>%
  pull(REF)

#==================================================================
# 2) CONSTRUCT (a) CTD STATIONS AND (b) CTD PROFILES TABLE
#==================================================================

#------------------------------------------------------------------
# GET VARIABLES NETCDF
#------------------------------------------------------------------

stations_table <- NULL #___(a)
profiles_table <- NULL #___(b)

files <- df_files %>% #___remove individuals with no ctd file
  filter(!is.na(ctd_file)) %>%
  pull(ctd_file)

i = 1

## Example netcdf dump
nc_open(files[1])

for (file in files) {
  
  print(i)
  
  ncdf <- nc_open(file) #___NETCDF file

  ## Reference
  ref <- ncatt_get(ncdf, varid = 0, attname = "smru_platform_code")
  ref <- ref$value
  ref <- gsub("_", "-", ref)
  
  ## Number stations and depths
  nprof <- ncdf$dim$N_PROF$len
  ndepth <- ncdf$dim$N_LEVELS$len
  
  ## Type
  # type = ???
  
  ## Time
  dates_j <- ncvar_get(ncdf, varid = "JULD") #___julian date: days since 1950-01-01 00:00:00 UTC
  dates <- as.POSIXct(as.Date(dates_j, origin = as.Date("1950-01-01 00:00:00")), tz = "UTC") #___POSIXct
  # days <- format(dates, format = "%m/%d/%Y") #___mm/dd/yyyy
  # time <- format(dates, format = "%H:%M") #___HH:MM
  
  ## Position: not corrected with state-space model
  lat <- ncvar_get(ncdf, varid = "LATITUDE") #___°N
  lon <- ncvar_get(ncdf, varid = "LONGITUDE") #___°E
  PQC <- ncvar_get(ncdf, varid = "POSITION_QC") %>% #___ARGOS QC
    list_to_array()
  
  #------------------------------------------------------------------
  # (a) CTD STATIONS TABLE: REF, station, date, time, lon, lat, PQC
  #------------------------------------------------------------------
  
  refs <- rep(ref, nprof)
  stations <- seq(1, nprof)
  
  stations_REF <- data_frame(REF = refs, 
                            station = stations,
                            time = dates,
                            lon = lon,
                            lat = lat,
                            PQC = PQC)
  
  stations_table <- rbind(stations_table, stations_REF)
  
  #----------------------------------------------------------------------------
  # (b) CTD PROFILES TABLE: REF, station, depth, temp, TQC, terr, sal, SQC, serr
  #----------------------------------------------------------------------------
  
  ## Temperature (°C)
  temp <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED") %>% #___temperature
    as_tibble() %>%
    gather() %>%
    pull(value)
  tqc <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED_QC") %>% #___temperature quality control
    list_to_array()
  terr <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED_ERROR") %>% #___temperature error: SEA TEMPERATURE ERROR IN SITU ITS-90 SCALE
    as_tibble() %>%
    gather() %>%
    pull(value)

  ## Salinity
  psal <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED") %>% #___practical salinity
    as_tibble() %>%
    gather() %>%
    pull(value)
  sqc <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED_QC") %>% #___practical salinity quality control
    list_to_array()
  serr <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED_ERROR") %>% #___practical salinity error: PRACTICAL SALINITY ERROR
    as_tibble() %>%
    gather() %>%
    pull(value)

  ## Depth 
  depth_array <- rep(seq(1:ndepth), nprof)
  # dqf <- ??????

  ## Stations
  station_array <- rep(1:nprof, each = ndepth)

  ## Reference
  ref_array <- rep(ref, nprof * ndepth)

  profiles_REF <- data_frame(REF = ref_array,
                   station = station_array,
                   depth = depth_array,
                   #DQC =
                   temp = temp,
                   TQC = tqc,
                   terr = terr,
                   psal = psal,
                   SQC = sqc,
                   serr = serr
                   )

  profiles_REF <- profiles_REF %>%
    na.omit()

  profiles_table <- rbind(profiles_table, profiles_REF)
  
  i = i + 1
  
}

stations_table <- stations_table %>%
  mutate(id_ctd = seq(1 ,nrow(stations_table))) #___add unique CTD profile identifier

saveRDS(stations_table, "~/Desktop/WHOI/Data/output_data/ctd_stations_table")
saveRDS(profiles_table, "~/Desktop/WHOI/Data/output_data/ctd_profiles_table")

## End script
rm(list=ls())
