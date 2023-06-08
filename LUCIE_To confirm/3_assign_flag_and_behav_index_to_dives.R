
rm(list=ls())

# ------------------------------------------------------------------------------
# ---------------------------------------------------------------- Load the data

#BSSM=read.csv("C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya_updated.csv",stringsAsFactors = F)
BSSM=read.csv("/run/media/lbourreau/Elements/Stage_Lucie/codes/ssm/filteredTracks_ssm_polynya_updated.csv",stringsAsFactors = F)
BSSM$date <- as.POSIXct(BSSM$date,origin="1970-01-01",tz="GMT")
str(BSSM)

#BSSM_beha=read.csv("C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya_behavior_updated.csv",stringsAsFactors = F)
BSSM_beha=read.csv("/run/media/lbourreau/Elements/Stage_Lucie/codes/ssm/filteredTracks_ssm_polynya_behavior_updated.csv",stringsAsFactors = F)
BSSM_beha$date <- as.POSIXct(BSSM_beha$date,origin="1970-01-01",tz="GMT")
str(BSSM_beha)

#dive_interpoled <- read.table("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela.txt", sep=",",header = TRUE)
dive_interpoled <- read.table("/run/media/lbourreau/Elements/Stage_Lucie/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_numpol21_shelf.txt", sep=",",header = TRUE)
dive_interpoled$date_fin <- as.POSIXct(dive_interpoled$date_fin,origin="1970-01-01",tz="GMT")
str(dive_interpoled)


#################### TEST

# dive_interpoled_sp <- dive_interpoled[which(dive_interpoled$id == "ct47-B-09"),]
# BSSM_sp <- BSSM[which(BSSM$id == "ct47-B-09"),]
# 
# # Convert dives and filtered tracks into a sp object
# coordinates(dive_interpoled_sp) <- ~lon+lat
# coordinates(BSSM_sp) <- ~lon+lat
# 
# # Calculate distance between points (i.e between dive and filtered track)
# dist <- gDistance(BSSM_sp, dive_interpoled_sp, byid = TRUE)
# min_dist <- apply(dist, 1, function(x) order(x, decreasing=F)[2])
# 
# # Find a boundary for the standard error associate to lat and lon of the filtered trajectories
# hist(BSSM[which(BSSM$x.se>25),]$x.se)
# hist(BSSM[which(BSSM$y.se>25),]$y.se)
# length(which(BSSM$x.se>10))
# length(which(BSSM$y.se>10))
# 
# test <- BSSM[-which(BSSM$x.se>10),]
# test <- test[-which(BSSM$y.se>10),]

#################### FIN TEST


# NE FONCTIONNE PAS CORRECTEMENT
# ind <- unique(dive_interpoled$id)
# 
# dive_interpoled$flag <- integer(dim(dive_interpoled)[1])
# dive_all <- data.frame()
# 
# for (ID in ind[1]){
#   print(ID)
#   
#   dive_interpoled_ind <- dive_interpoled[which(dive_interpoled$id == ID),]
#   dive_interpoled_ind$date_fin <- format(dive_interpoled_ind$date_fin,"%m-%d-%Y")
#   BSSM_ind <- BSSM[which(BSSM$id == ID),]
#   BSSM_ind$date <- format(BSSM_ind$date,"%m-%d-%Y")
#   
#   #dive_interpoled_sp <- dive_interpoled_ind
#   #BSSM_sp <- BSSM_ind
#   
#   #Boucle sur les jours
#   for (d in unique(dive_interpoled_ind$date_fin)){
#     print(d)
#     BSSM_ind_date <- BSSM_ind[which(BSSM_ind$date == d),]
#     dive_interpoled_ind_date <- dive_interpoled_ind[which(dive_interpoled_ind$date_fin == d),]
#     
#     dive_interpoled_sp <- dive_interpoled_ind_date
#     BSSM_sp <- BSSM_ind_date
#     
#     #Convertir en format sp
#     coordinates(dive_interpoled_sp) <- ~lon+lat
#     coordinates(BSSM_sp) <- ~lon+lat
#     
#     #Calcul de la distance entre track et dive et s?lection de la plus petite
#     dist <- gDistance(BSSM_sp, dive_interpoled_sp, byid = TRUE)
#     min_dist <- apply(dist, 1, function(x) order(x, decreasing=F)[2])
#     
#     #Tester la condition sur la standard error et attribuer 0 ou 1
#     for (j in min_dist){
#       if ((BSSM_ind_date$x.se[j]<30) & (BSSM_ind_date$y.se[j]<30)){
#         dive_interpoled_ind_date$flag[j] <- 1
#       }
#     }
#     dive_all <- rbind(dive_all,dive_interpoled_ind_date)
#   }
# }


# ------------------------------------------------------------------------------
# ---------------------------------------------------- Define threshold for s.e.

hist(BSSM$x.se, breaks = 5000, xlim = c(25,50))
hist(BSSM$y.se, breaks = 5000, xlim = c(0,50))

abline(v=25, col = "red")

# ------------------------------------------------------------------------------
# ---- Loop to assign flag 0 or 1 depending on the quality of the filtered track

install.packages("sp")
install.packages("rgeos")
install.packages("geosphere")
install.packages("survival")
install.packages("birk")

library(sp)
library(rgeos)
library(geosphere)
library(survival)
library(birk)

# dive_interpoled$flag <- integer(dim(dive_interpoled)[1])
dive_interpoled$gamma <- NA
dive_interpoled$se.gamma <- NA

dive_all <- data.frame()

dv <- dive_interpoled
bm <- BSSM

# BSSM$date <- format(BSSM$date,"%m-%d-%Y")

for (i in 1:nrow(dive_interpoled)){
  #print(i)
  
  # date <- format(dive_interpoled$date_fin[i],"%m-%d-%Y")
  # 
  # BSSM_date <- BSSM[which(BSSM$date == date & BSSM$id %in% dive_interpoled$id[i]),]
  # 
  # #Calcul de la distance entre track et dive et s?lection de la plus petite
  # dist <- distGeo(dive_interpoled[i,25:26],BSSM_date[,3:4])
  # min_dist <- which.min(dist)
  # 
  # #Tester la condition sur la standard error et attribuer 0 ou 1
  # if ((BSSM_date$x.se[min_dist]<25) & (BSSM_date$y.se[min_dist]<25)){
  #   dive_interpoled$flag[i] <- 1
  # }
  
  # Attribuer l'indice gamma (et s.e.) ? la plong?e suivant la date
  date_time <- dive_interpoled$date_fin[i]
  BSSM_beha_ind <- BSSM_beha[which(BSSM_beha$id %in% dive_interpoled$id[i]),]
  min_time <- which.closest(date_time,BSSM_beha_ind$date)
  dive_interpoled$gamma[i] <- BSSM_beha$g[min_time]
  dive_interpoled$se.gamma[i] <- BSSM_beha$g.se[min_time]
  
  dive_all <- rbind(dive_all,dive_interpoled[i,])
}

write.table(dive_all, file="/run/media/lbourreau/Elements/Stage_Lucie/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_flagTEST.txt",sep=",",row.names=F)
