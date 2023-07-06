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

for (file in files) {
  
  print(i)
  
  ncdf <- nc_open(file) #___NETCDF file

  ## Reference
  ref <- ncatt_get(ncdf, varid = 0, attname = "smru_platform_code")
  ref <- ref$value
  
  ## Number stations and depths
  nprof <- ncdf$dim$N_PROF$len
  ndepth <- ncdf$dim$N_LEVELS$len
  
  ## Type
  # type = ???
  
  ## Temperature (°C)
  temp <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED") %>% #___temperature
    as_tibble() %>%
    gather() %>%
    pull(value)
  tqc <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED_QC") %>% #___temperature quality control
    list_to_array()
  tsd <- ncvar_get(ncdf, varid = "TEMP_ADJUSTED_ERROR") %>% #___temperature standard deviation
    as_tibble() %>%
    gather() %>%
    pull(value)
  
  ## Salinity
  sal <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED") %>% #___salinity
    as_tibble() %>%
    gather() %>%
    pull(value)
  sqc <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED_QC") %>% #___salinity quality control
    list_to_array()
  ssd <- ncvar_get(ncdf, varid = "PSAL_ADJUSTED_ERROR") %>% #___salinity standard deviation
    as_tibble() %>%
    gather() %>%
    pull(value)
  
  ## Time
  dates_j <- ncvar_get(ncdf, varid = "JULD") #___julian date: days since 1950-01-01 00:00:00 UTC
  dates <- as.POSIXct(as.Date(dates_j, origin = as.Date("1950-01-01 00:00:00")), tz = "UTC") #___POSIXct
  days <- format(dates, format = "%m/%d/%Y") #___mm/dd/yyyy
  time <- format(dates, format = "%H:%M") #___HH:MM
  
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
                            date = days,
                            time = time,
                            lon = lon,
                            lat = lat,
                            PQC = PQC)
  
  #----------------------------------------------------------------------------
  # (b) CTD PROFILES TABLE: REF, station, depth, temp, TQC, tsd, sal, SQC, ssd
  #----------------------------------------------------------------------------
  
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
                   tsd = tsd,
                   sal = sal,
                   SQC = sqc,
                   ssd = ssd
                   )
  
  profiles_REF <- profiles_REF %>%
    na.omit()
  
  stations_table <- rbind(stations_table, stations_REF)
  profiles_table <- rbind(profiles_table, profiles_REF)
  
  i = i + 1
  
}

saveRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table", stations_table)
saveRDS("~/Desktop/WHOI/Data/output_data/ctd_profiles_table", profiles_table)









profile <- data_frame(t = temp[,1], s = sal[,1], depth = seq(1:1000))

profile <- profile %>%
  filter(!is.na(t))

plot(profile$t, -profile$depth)




## Import CTD data

ctd <- read.delim("~/Dropbox/data/data_oceanographic/ct109/ct109-939-14_ODV.txt", header = F, skip = 2)
names(ctd) <- c("Cruise",	"Station",	"Type",	"mon/day/yr",	"hh:mm", "Longitude",	"Latitude",	"Depth",	"DQF", "Temperature",	"TQF",	"Salinity", "SQF", "ncdf")

ctd <- ctd %>%
  filter(Station == 1)

plot(ctd$Temperature, -ctd$Depth)

CTD <- read.delim("~/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/2_data_csv_interp/ct128-246BAT-12_ODV.txt", header=T)

CTD <- read.delim("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/data_oceanographic/ct36/ct36-A-09_ODV.txt", header=T)

# names(data1)<-c("Cruise",	"Station",	"Type",	"mon/day/yr",	"hh:mm", "Longitude",	"Latitude",	"Depth",	"QF",
#                 "Temperature",	"QF",	"Salinity", "QF")

# data1<-nc_open("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/4_data_netcdf_interp/ct128-246BAT-12_hr1_prof.nc")
# print(data1)
# lat <- ncvar_get(data,"Y_COORD")nlat <- dim(lat)head(lat)
# lon <- ncvar_get(data,"X_COORD")nlon <- dim(lon)head(lon)

a=which(nchar(CTD$mon_day_yr)==7)
CTD$mon_day_yr[a]=paste(0,CTD$mon_day_yr[a],sep="")
a=which(nchar(CTD$mon_day_yr)==9)
#CTD$date_ctd[a]=paste(0,CTD$date_ctd[a],sep="")

a=which(nchar(CTD$mon_day_yr)==8)

library(stringr)
CTD$mon_day_yr[a]=str_replace(CTD$mon_day_yr[a], "/17", "/2017")

CTD$date_ctd <- paste(CTD$mon_day_yr, CTD$hh_mm,sep=" ")
CTD$date_ctd <- as.character(CTD$date_ctd)
CTD$date_ctd <- strptime(CTD$date_ctd, format="%m/%d/%Y %H:%M")

c=which(CTD$Depth==0)
c=which(CTD$Salinity==0)

CTD$date=as.character(CTD$date_ctd)
unic=unique(CTD$Station)

#on enleve les stations où on a une seule valeur
data1=data.frame()

for (i in unic){
  dt=CTD[CTD$Station==i,]
  if (nrow(dt) > 1){
    data1=rbind(data1,dt)
  }
}
CTD=as.data.frame(data1)

CTD$date_ctd=as.POSIXct(CTD$date_ctd,origin="1970-01-01",tz="GMT")
CTD$month=as.numeric(format(CTD$date_ctd,format="%m"))
CTD$id="ct128-246BAT-12"
CTD=CTD[,c(1,2,3,4,5,6,7,8,10,12,14,16,17)]
names(CTD)=c("Cruise", "Station", "Type", "mon.day.yr", "hh.mm", "longitude", 
             "latitude", "depth", "temp", "salinity", "date","month","id")
ctd1=CTD




## End script
rm(list=ls())
