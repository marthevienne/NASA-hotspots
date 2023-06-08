##
## Script name: hunting_time_benthic_pelagic
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-08
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

## Working directory
setwd("~/Desktop/WHOI/Data/")

## Path to save output figures
path_output = ("~/Dropbox/data/outputs_Marthe_2023/dive_mode/")

## Library
library(ggplot2)

## Import dive data
#dives = readRDS("interpoled_locations_dives_north_boundary_5meters")
dives = readRDS("filtered_dives_north_boundary_5meters_bathy") # pour l'instant - positions pas corrigées
dives$DE_DATE = as.POSIXct(dives$DE_DATE,origin="1970-01-01",tz="GMT")

#==================================================================
# 1) DIVES BELOW BATHYMETRY
#==================================================================

# Select only dives with valid bathy
dives_bis <- dives[which(dives$bathy != -9999),]
dives_bis$Bathy <- abs(dives_bis$bathy)
dives_bis$dist_to_sea_floor <- dives_bis$Bathy - dives_bis$MAX_DEP
dives_above_seafloor <- dives_bis[dives_bis$dist_to_sea_floor < 0, ]

perc = nrow(dives_above_seafloor)/nrow(dives_bis) * 100

#-----Save output
sink("pelag_benth_dives_output.txt")
cat(paste("Last modified :", Sys.Date()))
cat("\n")
cat(sprintf("Percentage of dives with max depth > bathymetry: %f %%", perc))
cat("\n")
sink()
#----------------

ggplot(data = dives_above_seafloor, mapping = aes(x = dist_to_sea_floor, y = bathy, color = REF)) +
  theme_bw() +
  geom_point(size = 2)+
  theme(legend.position = "none")

ggplot(data = dives_above_seafloor, mapping = aes(x = START_LON, y = START_LAT, color = REF)) +
  theme_bw() +
  geom_point(size=2) + 
  theme(legend.position = "none")

ggplot(data = dives_above_seafloor, mapping = aes(x = START_LON, y = START_LAT, color = dist_to_sea_floor)) +
  theme_bw() +
  geom_point(size = 2)+
  labs(x="Longitude (°E)", y="Latitude (°N)",colour="Depth difference (m)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"))


tiff(
  paste0(path_output, "hist_dive_above_bathy.tiff"),
  res = 700,
  height = 14,
  width = 24,
  units = "cm",
  compression = "lzw"
)
hist(
  dives_bis$dist_to_sea_floor,
  freq = T,
  breaks = 200,
  xlim = c(-800, 5000),
  main = "",
  xlab = "Difference between bathymetry and maximum depth of the dive"
)

abline(v = 70, col = "red")
abline(v = 50, col = "black")
abline(v = 100, col = "blue")
dev.off()

plot(density(dives_bis$dist_to_sea_floor),xlim=c(-1000,5000))
abline(v = 70, col="red")
