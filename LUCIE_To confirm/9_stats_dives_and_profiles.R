
rm(list=ls())

library(dplyr)
library(sf)
library(tidyverse)
library(sp)
library(raster)

mat_all <- read.table("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/CTD/PCA_mat_all_NumPolCorr_bathy.txt", sep=",", header=TRUE)

dives <- read.table("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMultBigest_shelf_BehaInd_MNDepHT.txt", sep=",", header=TRUE)

CTD_all <- read.table("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/CTD/NN_mat_all_ind_170m_maxdep_MLD_new_ssm_NumPol_noNULL_Diveassign_south585_bathy.txt", sep=",", header=TRUE)

### ------------- Nb profiles within each polynya and time spent within polynyas


tab_time <- data.frame("Pol9"=rep(0,9), "Pol11"=rep(0,9), "Pol12"=rep(0,9), "Pol14"=rep(0,9))
tab_nbprof <- data.frame("Pol9"=rep(0,9), "Pol11"=rep(0,9), "Pol12"=rep(0,9), "Pol14"=rep(0,9))

for(i in unique(mat_all$id)){
  data <- mat_all[which(mat_all$id == i),]
  
  data9 <- data[which(data$NumPol == 9),]
  data11 <- data[which(data$NumPol == 11),]
  data12 <- data[which(data$NumPol == 12),]
  data14 <- data[which(data$NumPol == 14),]
  
  tab_nbprof$Pol9[i] = nrow(data9)
  tab_nbprof$Pol11[i] = nrow(data11)
  tab_nbprof$Pol12[i] = nrow(data12)
  tab_nbprof$Pol14[i] = nrow(data14)
  
  tab_time$Pol9[i] = length(unique(data9$doy))
  tab_time$Pol11[i] = length(unique(data11$doy))
  tab_time$Pol12[i] = length(unique(data12$doy))
  tab_time$Pol14[i] = length(unique(data14$doy))
  
}

### ------------ Nb profiles of fluo profiles deleted because NA between 10-170m

mat2 <- mat_all[,45:205]
ToDelete <- which(is.na(mat2), arr.ind = TRUE)
ToDelete <- unique(ToDelete[,1])
length(ToDelete)*100/nrow(mat_all) # Percentage of deleted profiles because of NA values between 10 and 170m

### -------------------------------------------------------------- Polynyas area

pol9_contours <- read.csv("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/Contours_pol9_pol14/Contours_Biggest/contours_pol9.csv", sep=";", header=TRUE)
pol14_contours <- read.csv("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/Contours_pol9_pol14/Contours_Biggest/contours_pol14.csv", sep=";", header=TRUE)
pol12_contours <- read.csv("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/Contours_pol9_pol14/Contours_Biggest/contours_pol12.csv", sep=";", header=TRUE)

# ---------- Test 1

pol9_contours <- na.omit(pol9_contours)

polygon9 <- pol9_contours %>%
  #group_by("id") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  summarise(geometry = st_combine(geometry), do_union=FALSE) %>%
  st_cast("POLYGON")
polygon9

plot(polygon9)

# ---------- Test 2

##### Polynya 9

pol9_contours <- na.omit(pol9_contours)

### Design the polygons from lon/lat

poly_list9 <- sapply(unique(pol9_contours$id), function(x){
  # create a polygon for each unique id
  poly = Polygon(pol9_contours[pol9_contours$id == x, 2:1]) 
  # create a spatial polygon from the polygon
  polys = SpatialPolygons(list(Polygons(list(poly), ID = x)), proj4string = CRS("+proj=longlat +datum=WGS84"))
  # convert the spatial polygons to spatial polygons dataframe
  as(polys, 'SpatialPolygonsDataFrame')
})

# rbind the list of spatial polygons dataframe

poly_list9 <- do.call(rbind, poly_list9)

# visualize 

plot(poly_list9)

### Compute the area in km2

area9 <- area(poly_list9)/10^6
area9

##### Polynya 14

pol14_contours <- na.omit(pol14_contours)

### Design the polygons from lon/lat

poly_list14 <- sapply(unique(pol14_contours$id), function(x){
  poly = Polygon(pol14_contours[pol14_contours$id == x, 2:1]) 
  polys = SpatialPolygons(list(Polygons(list(poly), ID = x)), proj4string = CRS("+proj=longlat +datum=WGS84"))
  as(polys, 'SpatialPolygonsDataFrame')
})


poly_list14 <- do.call(rbind, poly_list14)


plot(poly_list14)

### Compute the area in km2

area14 <- area(poly_list14)/10^6
area14


##### Polynya 12

pol12_contours <- na.omit(pol12_contours)

### Design the polygons from lon/lat

poly_list12 <- sapply(unique(pol12_contours$id), function(x){
  poly = Polygon(pol12_contours[pol12_contours$id == x, 2:1]) 
  polys = SpatialPolygons(list(Polygons(list(poly), ID = x)), proj4string = CRS("+proj=longlat +datum=WGS84"))
  as(polys, 'SpatialPolygonsDataFrame')
})


poly_list12 <- do.call(rbind, poly_list12)


plot(poly_list12)

### Compute the area in km2

area12 <- area(poly_list12)/10^6
area12


### ---------------------------------------------------------------- Basic stats


tab_final <- data.frame("id"="", "NbDives"=0, "NbDivesPol"=0, "Pol"="", "MeanDepth"=0, "SDdepth"=0,
                  "MeanDur"=0, "SDdur"=0, "MeanHT"=0, "SDHT"=0,
                  "MeanMDHT"=0, "SDMDHT"=0)

for(i in unique(dives$id)){
  tab <- data.frame("id"="", "NbDives"=0, "NbDivesPol"=0, "Pol"="", "MeanDepth"=0, 
                    "SDdepth"=0, "MeanDur"=0, "SDdur"=0, "MeanHT"=0, "SDHT"=0,
                    "MeanMDHT"=0, "SDMDHT"=0)
  
  data <- dives[which(dives$id == i),]
  
  tab$id <- i
  
  tab$NbDives <- nrow(data)
  tab$NbDivesPol <- length(which(data$pol != 0))
  
  visitedPol <- unique(data$pol)
  tab$Pol <- paste(visitedPol, collapse = " - ")
  
  tab$MeanDepth <- mean(data$max_dep, na.rm = TRUE)
  tab$SDdepth <- sd(data$max_dep, na.rm = TRUE)
  
  tab$MeanDur <- mean(data$dive_dur/60, na.rm = TRUE)
  tab$SDdur <- sd(data$dive_dur/60, na.rm = TRUE)
  
  tab$MeanHT <- mean(data$hunting, na.rm = TRUE)
  tab$SDHT <- sd(data$hunting, na.rm = TRUE)
  
  tab$MeanMDHT <- mean(data$MeanDepthHT, na.rm = TRUE)
  tab$SDMDHT <- sd(data$MeanDepthHT, na.rm = TRUE)
  
  tab_final <- rbind(tab_final,tab)
}

tab_final <- tab_final[-1,]

write.table(tab_final, "C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/stats_all_ind_tableau_propre.txt", sep=",", row.names = FALSE)

tab <- read.table("C:/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/codes/data_SES_polynya/stats_all_ind_tableau_propre.txt", sep=",",header = TRUE)

max(tab$MeanDepth)
min(tab$MeanDepth)

max(tab$MeanDur)
min(tab$MeanDur)


test <- dives[-which(dives$lat > -58.5),]

table(dives$pol)

####### Longueur des trajets en mois

library(lubridate)

dives$month <- month(dives$date_fin)

tab <- table(dives$id,dives$month)
tab <- as.data.frame.matrix(tab)

nb <- rowSums(test != 0)
nb_vec <- as.vector(nb)

min(nb_vec)
max(nb_vec)
mean(nb_vec)
sd(nb_vec)

### ----------------------------------------------------- Stats tous les profils

# Nb of profiles per day

CTD_all$dateYMD <- as.Date(CTD_all$date, "%Y-%m-%d")

tab1 <- data.frame("id"="", "dateYMD"="", "nbprof"=0)

c=0
for(a in unique(CTD_all$id)){
  c = c+1
  df <- CTD_all[which(CTD_all$id == a),]
  for(b in unique(df$dateYMD)){
    df2 <- df[which(df$dateYMD == b),]
    tab2 <- data.frame("id"="", "dateYMD"="", "nbprof"=0)
    tab2$id <- a
    tab2$dateYMD <- b
    tab2$nbprof <- nrow(df2)
    tab1 <- rbind(tab1,tab2)
  }
}
tab1 <- tab1[-1,]
mn <- mean(tab1$nbprof)
sd <- sd(tab1$nbprof)

# Delete profiles that still have NA

ind_fluo <- c(22314,22332,21384,81330, 81336,204734,204737,66996,89256)
CTD_all <- CTD_all[-which(CTD_all$id %in% ind_fluo),] # Remove profiles of the individuals that already have fluorescence


CTD_all_noNA <- CTD_all %>% drop_na(c(13:352)) # Remove the SAL and TEMP profiles with NA because the prediction can't be done with NA

# Number of profiles within polynyas

length(which(CTD_all_noNA$pol == 1))

table(CTD_all_noNA$NumPol)

