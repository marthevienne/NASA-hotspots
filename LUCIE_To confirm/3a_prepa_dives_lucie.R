#3------------------------------------------------------------------------------
########################### Prepa fichiers dive ################################

rm(list=ls())

#-------------------------------------------------------------------------------

# Compile dive file if more than one part and save it as a .xlsx ---------------
# 
# library(readxl)
# library(writexl)
# 
# ct77_dive_part1 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct77_dive_part1.xlsx", sep=""))
# ct77_dive_part2 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct77_dive_part2.xlsx", sep=""))
# ct77_dive_part3 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct77_dive_part3.xlsx", sep=""))
# 
# ct77_dive <- rbind(ct77_dive_part1,ct77_dive_part2,ct77_dive_part3)
# write_xlsx(ct77_dive, path = tempfile(fileext = ".xlsx")) # Sauvegarde en .xlsx dans un fichier fantome à rechercher sur l'ordi
# 
# ct78_dive_part1 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct78_dive_part1.xlsx", sep=""))
# ct78_dive_part2 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct78_dive_part2.xlsx", sep=""))
# ct78_dive <- rbind(ct78_dive_part1,ct78_dive_part2)
# write_xlsx(ct78_dive, path = tempfile(fileext = ".xlsx")) # Sauvegarde en .xlsx dans un fichier fantome à rechercher sur l'ordi
# 
# ct78d_dive_part1 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct78d_dive_part1.xlsx", sep=""))
# ct78d_dive_part2 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct78d_dive_part2.xlsx", sep=""))
# ct78d_dive <- rbind(ct78d_dive_part1,ct78d_dive_part2)
# write_xlsx(ct78d_dive, path = tempfile(fileext = ".xlsx")) # Sauvegarde en .xlsx dans un fichier fantome à rechercher sur l'ordi
# 
# ct148_dive_part1 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct148_dive_part1.xlsx", sep=""))
# ct148_dive_part2 <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/ct148_dive_part2.xlsx", sep=""))
# ct148_dive <- rbind(ct148_dive_part1,ct148_dive_part2)
# write_xlsx(ct148_dive, path = tempfile(fileext = ".xlsx")) # Sauvegarde en .xlsx dans un fichier fantome à rechercher sur l'ordi

# End compilation --------------------------------------------------------------
# ------------------------------------------------------------------------------


# Start prepa dives ------------------------------------------------------------

# Creation of a data with all the dives of all the individuals (independent of
# the deployment number)

library(dplyr)
library(readxl)

list_deployments <- matrix(c("ct3","ct7","ct34","ct36","ct47","ct64","ct78d","ct75",
                             "ft07","ct77","ct78","ct96","ct84","ct98","ct111","ct109",
                             "ct120","ct116","ct121","ct134","ct128","ct133","ct135",
                             "ct140","ct144","ct148","ct149","ft24","ct160","ct157"),
                           nrow=30,ncol=1)

data <- NULL
for (i in list_deployments){
  print(i)
  deployment <- read_excel(paste("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/diag_dive_ind_pol/",i,"_dive.xlsx", sep=""))
  deployment <- rename_with(deployment, toupper)
  deployment <- cbind.data.frame(REF=deployment$REF, DE_DATE=as.POSIXct(deployment$DE_DATE,origin="1970-01-01",tz="GMT"), SURF_DUR=deployment$SURF_DUR,
                      DIVE_DUR=deployment$DIVE_DUR, MAX_DEP=deployment$MAX_DEP,
                      D1=deployment$D1, D2=deployment$D2, D3=deployment$D3, D4=deployment$D4,
                      T1=deployment$T1, T2=deployment$T2, T3=deployment$T3, T4=deployment$T4,
                      LAT=deployment$LAT, LON=deployment$LON,
                      START_LAT=deployment$START_LAT, START_LON=deployment$START_LON)
  data <- rbind(data,deployment)
}
str(data)

data$date=as.POSIXct(data$DE_DATE,origin="1970-01-01",tz="GMT")
data$LAT=as.numeric(gsub(",",".",data$LAT))
data$LON=as.numeric(gsub(",",".",data$LON))
data$START_LAT=as.numeric(gsub(",",".",data$START_LAT))
data$START_LON=as.numeric(gsub(",",".",data$START_LON))

str(data)

# Très long à sauvegarder
write.csv(data, file="/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dives_compile.csv",row.names=T)

data <- read.csv("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dives_compile.csv", stringsAsFactors = FALSE)
# Loop to assign a number to each dive in order to keep the dive number
# even if we remove some dives

unik=unique(data$REF)
data$num=NA
data$new=0

for (p in 1:length(unik)){
  print(p)
  sa <- data[data$REF==unik[p],]
  sa <- sa[with(sa, order(sa$date)), ]
  sa$num <- rep(1:nrow(sa),1)
  sa$new=1
  data<-rbind(data,sa)
}
a=which(data$new==0)
data=data[-a,]
  
#select only data south of a boundary
select=which(data$LAT < -58.5)
data=data[select,]

# On supprime les donnees manquantes--------------------------------------------

a=which(nchar(data$DE_DATE)==2) #on enleve les plongees pour lesquelles on a une date de fin de plongee == NA
#data=data[-a,]
ddr <-which(data$DIVE_DUR == 0)   #on enleve les plongees pour lesquelles on a une duree de plongee == 0
#data=data[-ddr,]
dt1 <- which(data$T1 == 0)   #on enleve les plongees pour lesquelles on a une duree a  T1 == 0
#data=data[-dt1,]

#il s'agit de pourcentages du temps de plongee pour chaque point donc ce % doit evoluer au cours de la plongee
x <- which(data$T1 == data$T2)
#data <-data[-x,]
y <- which(data$T2 == data$T3)
#data <-data[-y,]
z <- which(data$T3 == data$T4)
#data <-data[-z,]

sup=which(data$T1 > data$T2)
#data <-data[-sup,]
sup2=which(data$T2 > data$T3)
#data <-data[-sup2,]
sup3=which(data$T3 > data$T4)
#data <-data[-sup3,]

b <- which(data$MAX_DEP >= 2000)
test=data[b,]
#data <- data[-b,]
# c <- which(data$MAX_DEP >= 1500)

write.table(data, file="/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dive_outliers_removed_SES_polynia_boundary.txt",sep=",",row.names=F)

data <- read.table("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dive_outliers_removed_SES_polynia_boundary.txt", sep=",", header = TRUE)
str(data)

data$date=as.POSIXct(data$DE_DATE,origin="1970-01-01",tz="GMT")

tot=data

#On calcule et on rentre des nouvelles donnees----------------------------------


data_all = NULL

for (individu in unique(tot$REF)){#list_ind){
  
  print(individu)
  a = which(tot$REF == individu)
  data = tot[a,]
  num = unique(data$num)
  dbs <- data.frame("num"=rep(0,1) ,"angle_sol"=0,"max_dep"=0, "dive_dur"=0, "surf_dur"=0,"date_fin"=0, "D0"=0,"D1"=0, "D2"=0, "D3"=0, "D4"=0, "Df"=0,"T0"=0,"T1"=0, "T2"=0,"T3"=0,"T4"=0,"Tf"=0,"bott_time"=0,"speed_desc"=0,"speed_asc"=0,"startlon"=0,"startlat"=0)  
  
  for (d in num){ 
    dt <- data[data$num==d,]
    if (dt$MAX_DEP >= 5) {
      dbs2 <- data.frame("num"=rep(0,1) ,"angle_sol"=0,"max_dep"=0, "dive_dur"=0, "surf_dur"=0,"date_fin"=0, "D0"=0,"D1"=0, "D2"=0, "D3"=0, "D4"=0, "Df"=0,"T0"=0,"T1"=0, "T2"=0,"T3"=0,"T4"=0,"Tf"=0,"bott_time"=0,"speed_desc"=0,"speed_asc"=0,"startlon"=0,"startlat"=0)  
      
      dbs2$num = d
      dbs2$angle_sol = NA
      dbs2$max_dep = dt$MAX_DEP
      dbs2$dive_dur = dt$DIVE_DUR
      dbs2$surf_dur = dt$SURF_DUR
      dbs2$date_fin = dt$date
      
      dbs2$D0 = 0
      dbs2$Df = 0
      dbs2$D1 = dt$D1
      dbs2$D2 = dt$D2
      dbs2$D3 = dt$D3
      dbs2$D4 = dt$D4 
      
      e <- dt$date-dt$DIVE_DUR
      f <- (dt$T1)*(dt$DIVE_DUR)/100
      g <- (dt$T2)*(dt$DIVE_DUR)/100
      h <- (dt$T3)*(dt$DIVE_DUR)/100
      i <- (dt$T4)*(dt$DIVE_DUR)/100
      j <- (dt$DIVE_DUR)
      
      dbs2$T0= as.numeric((ISOdatetime(1970,1,1,0,0,0,tz="gmt")+ as.numeric(e)))
      dbs2$T1= as.numeric((e + f))
      dbs2$T2= as.numeric((e + g))
      dbs2$T3= as.numeric((e + h))
      dbs2$T4= as.numeric((e + i))
      dbs2$Tf= as.numeric((e + j))
      
      ref <- c(dbs2$D0,dbs2$D1,dbs2$D2,dbs2$D3,dbs2$D4,dbs2$Df) 
      tim <- c(dbs2$T0,dbs2$T1,dbs2$T2,dbs2$T3,dbs2$T4,dbs2$Tf)
      tim=as.numeric(tim)
      st <- seq(dbs2$T0, dbs2$Tf,2)
      
      interp <- approx(tim,ref,xout=st, method="linear")
      bottdep <- (0.8)*(dt$MAX_DEP)
      t <- c(interp$x[interp$y >= bottdep])
      
      if (length(t)==0){
        bottom_time <- NA
        dbs2$bott_time= as.numeric(bottom_time)
        dbs2$speed_desc= NA
        dbs2$speed_asc= NA
      } else {
        bottom_time <- t[length(t)]-t[1]
        difft1 <- as.numeric(t[1]-dbs2$T0)
        diffd1 <- (interp$y[interp$x == t[1]])-(dbs2$D0)
        difft2 <- as.numeric(dbs2$Tf-t[length(t)])
        diffd2 <- -(dbs2$Df-(interp$y[interp$x == t[length(t)]]))
        dbs2$bott_time= as.numeric(bottom_time)
        
        dbs2$speed_desc= diffd1/difft1
        dbs2$speed_asc= diffd2/difft2
        }
      
      dbs2$startlon=dt$START_LON
      dbs2$startlat=dt$START_LAT
      
      dbs <- rbind(dbs,dbs2)
    }else next
  }
  dbs <- dbs[-1,]
  
  dataID=dbs
  dataID$id=individu
  data_all=rbind(data_all, dataID)
}

write.table(data_all, file="/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dive_outliers_removed_SES_polynia_boundary_newdata_5meters.txt",sep=",",row.names=F)

# Stat--------------------------------------------------------------------------

data=read.table("C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult_shelf_BehaInd_MNDepHT.txt",sep=",",head=T)

unik=unique(data$id)

data$date_fin=as.POSIXct(data$date_fin,origin="1970-01-01",tz="GMT")
data$dateR=as.Date(data$date_fin)

loc2 <- NULL
md <- NULL
mdur=NULL

dbs <- data.frame("ref"=rep(0,1) ,"m_depth"=0,"sd_depth"=0, "m_dur"=0, "sd_dur"=0,
                   "nb_loc"=0,"sd_loc"=0,"max_depth"=0,"min_depth"=0)   

for (p in 1:length(unik)){
  print(p)
  dbs2 <- data.frame("ref"=rep(0,1) ,"m_depth"=0,"sd_depth"=0, "m_dur"=0, "sd_dur"=0,
                     "nb_loc"=0,"sd_loc"=0,"max_depth"=0,"min_depth"=0) 
  
  sa <- data[data$id==unik[p],]
  
  dbs2$max_depth <- max(sa$max_depth) # profondeur plongée la plus profonde
  dbs2$min_depth <- min(sa$max_depth) # profondeur plongée la moins profonde
  dbs2$m_depth <- mean(sa$max_dep)#  moyenne de temps entre les locs en hours
  dbs2$sd_depth <- sd(sa$max_dep)# ecart-types en hours
  md <- c(md,sa$max_dep) # on garde tous les Ã©carts de temps pour faire une moy sur tous les ind, en sec
  dbs2$ref=as.character(sa$id[1])
  
  dbs2$m_dur=mean(sa$dive_dur)/60
  dbs2$sd_dur=sd(sa$dive_dur)/60
  mdur <- c(mdur,sa$dive_dur)
  
  unic=unique(as.numeric(sa$dateR))  
  loc <- data.frame()
  
  for (i in unic) {
    
    dataOneDay <- sa[which(sa$dateR == i),]
    locperDay <- length(dataOneDay$startlat)
    loc <- rbind(loc, locperDay)
    loc2 <- c(loc2,locperDay)
  }
  
  dbs2$nb_loc=sapply(loc,mean)
  dbs2$sd_loc=sapply(loc,sd)
  dbs=rbind(dbs,dbs2)
}
dbs=dbs[-1,]

# Save dbs as a .csv file in Dropbox -> dives non réinterpolées avec les tracks filtrés
write.csv(dbs, file="C:/Users/lucie/Dropbox/codes/data_SES_polynya/stat_dive_outliers_removed_boundary_non_interpoled_loc_5meters.csv",row.names=T)

md <- as.data.frame(md)#Profondeur max des dives
(sapply(md, mean)) 
(sapply(md, sd)) 

md_dur <- as.data.frame(mdur)#Durée des dives
(sapply(md_dur, mean)/60) 
(sapply(md_dur, sd)/60) 

loc2=as.data.frame(loc2)# nb de locs par jour pour tous
sapply(loc2, mean)
sapply(loc2, sd) 

# Stats pour nb de plongées dans une polynie et nb de plongées total

dbs3 <- data.frame("ref"=rep(0,1),"nb_dives_pol"=0 ,"nb_dives"=0)  

for (seal in 1:length(unique(data$id))){
  data_ind <- data[data$id == unique(data$id)[seal],] # selectionner dives de l'individu
  dbs4 <- data.frame("ref"=rep(0,1),"nb_dives_pol"=0 ,"nb_dives"=0) # tableau vide pour cet individu
  
  dbs4$ref=as.character(data_ind$id[1])
  dbs4$nb_dives_pol = length(which(data_ind$pol != 0))
  dbs4$nb_dives = dim(data_ind)[1]
  
  dbs3 = rbind(dbs3,dbs4)
}
dbs3=dbs3[-1,]

write.csv(dbs3, file="C:/Users/lucie/Dropbox/codes/data_SES_polynya/statNumPol_dive_outliers_removed_boundary_non_interpoled_loc_5meters.csv",row.names=T)


################################################################################
# Apres avoir fait tourner le BSSM on reinterpole les locs, 

rm(list=ls())

dive <- read.table("/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/data_Lucie/all_dive_outliers_removed_SES_polynia_boundary_newdata_5meters.txt",sep=",",head=T)
dive$date_fin <- as.POSIXct(dive$date_fin,origin="1970-01-01",tz="GMT")
str(dive)

BSSM=read.csv("C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya_updated.csv",stringsAsFactors = F)
BSSM$date <- as.POSIXct(BSSM$date,origin="1970-01-01",tz="GMT")
str(BSSM)

#----------------------------------reinterpole les locs dive ac le bssm
dt1 = NULL
seal=unique(BSSM$id)

#we remove the part where the bssm did not work and keep the raw data

# a=which(dive$id=="wd09-414-16" & as.numeric(format(dive$date,"%m"))>=6)
# dt2=dive[a,]
# dive=dive[-a,]

for (p in 1:length(seal)){
  print(p)
  sa <- dive[dive$id==seal[p],]
  bssm <- BSSM[BSSM$id==seal[p],]
  
  interp1 <- approx(bssm$date,bssm$lon,xout=sa$date_fin)
  sa$lon <- interp1$y
  interp2 <- approx(bssm$date,bssm$lat,xout=sa$date_fin)
  sa$lat <- interp2$y
  
  dt1 <- rbind(dt1,sa)
}

# dt2$lon=dt2$startlon
# dt2$lat=dt2$startlat
# 
# all=rbind(dt1,dt2)

write.table(dt1, file="C:/Users/lucie/Dropbox/codes/filtered_dive_ses_polynya_5meters.txt",sep=",",row.names=F)


