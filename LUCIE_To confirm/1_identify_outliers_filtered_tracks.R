rm(list=ls())

filtered_tracks <- read.csv("C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya1.csv", stringsAsFactors = F)
non_filtered_tracks <- read.csv("C:/Users/lucie/Dropbox/codes/compile_diag_non_filtered_tracks.csv", header = TRUE)
data_dive <- read.table("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dive_outliers_removed_SES_polynia_boundary_PolNum.txt", header = TRUE, sep=",")


filtered_tracks$date=as.POSIXct(filtered_tracks$date,origin="1970-01-01",tz="GMT")
str(filtered_tracks)

non_filtered_tracks$D_DATE=as.POSIXct(non_filtered_tracks$D_DATE,origin="1970-01-01",tz="GMT")
non_filtered_tracks$LAT=as.numeric(unlist(strsplit((gsub(",",".",(as.character(non_filtered_tracks$LAT)))),",")))
non_filtered_tracks$LON=as.numeric(unlist(strsplit((gsub(",",".",(as.character(non_filtered_tracks$LON)))),",")))
str(non_filtered_tracks)

# Plot zoom filtered VS non filtered tracks per individual
library(scales)

ID = "ct47-B-09"

filtered_ID <- filtered_tracks[which(filtered_tracks$id == ID),]
non_filtered_ID <- non_filtered_tracks[which(non_filtered_tracks$ref == ID),]

filtered_ID <- filtered_ID[which(filtered_ID$lat < -65),]
non_filtered_ID <- non_filtered_ID[which(non_filtered_ID$LAT < -66),]

filtered_ID <- filtered_ID[which(filtered_ID$lon > 60),]
non_filtered_ID <- non_filtered_ID[which(non_filtered_ID$LON > 60),]

plot(filtered_ID$lon,filtered_ID$lat,col=alpha("black",0.3),pch=16,xlab = "LON",ylab = "LAT",main = ID)
points(non_filtered_ID$LON,non_filtered_ID$LAT,col=alpha("red",0.3),pch=16)

# Test if the individual spend a lot of time inside polynya

library(dplyr)

ID = "ft07-Cy29-11"

pol_count = data_dive %>% count(id, pol, sort = TRUE)
pol_count = pol_count[-which(pol_count$pol == 0),] # 0 correponds to no polynia so we delete this lines

pol_count[which(pol_count$id == ID),]
