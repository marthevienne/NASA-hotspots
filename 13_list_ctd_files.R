## ---------------------------
##
## Script name: list_ctd_files
##
## Purpose of script:
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
library(stringr)
## ---------------------------
## Paths
path_ctd <- "~/Dropbox/data/data_oceanographic/"
## ---------------------------
## Functions
source("~/Desktop/WHOI/Codes/functions_raster/netcdf_to_raster.R")
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

type = "ODV"
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
# 2) OPEN NETCDF FILE 
#==================================================================

## Test with one file
file <- df_files$ctd_file[1]
toto <- nc_open(file)

## Temp
temp <- ncvar_get(toto, varid = "TEMP_ADJUSTED")
tqc <- ncvar_get(toto, varid = "TEMP_ADJUSTED_QC")
tsd <- ncvar_get(toto, varid = "TEMP_ADJUSTED_ERROR")

## Salinity
sal <- ncvar_get(toto, varid = "PSAL_ADJUSTED")
sqc <- ncvar_get(toto, varid = "PSAL_ADJUSTED_QC")
ssd <- ncvar_get(toto, varid = "PSAL_ADJUSTED_ERROR")

## Time
dates_j <- ncvar_get(toto, varid = "JULD")
dates <- as.POSIXct(as.Date(dates_j, origin = as.Date("1950-01-01 00:00:00")), tz = "UTC")
days <- format(dates, format = "%m/%d/%Y")
time <- format(dates, format = "%H:%M")

## Position
lat <- ncvar_get(toto, varid = "LATITUDE")
lon <- ncvar_get(toto, varid = "LONGITUDE")
PQC <- ncvar_get(toto, varid = "POSITION_QC")

## Type
# type = ???

## Gather data in dataframe
t_array <- temp %>%
  as_data_frame() %>%
  gather() %>%
  pull(value)

sal_array <- sal %>%
  as_data_frame() %>%
  gather() %>%
  pull(value)








profile <- data_frame(t = temp[,1], s = sal[,1], depth = seq(1:1000))

profile <- profile %>%
  filter(!is.na(t))

plot(profile$t, -profile$depth)




## Import CTD data

ctd <- read.delim("~/Dropbox/data/data_oceanographic/ct109/ct109-939-14_ODV.txt", header = F, skip = 2)
names(ctd) <- c("Cruise",	"Station",	"Type",	"mon/day/yr",	"hh:mm", "Longitude",	"Latitude",	"Depth",	"DQF", "Temperature",	"TQF",	"Salinity", "SQF", "toto")

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
