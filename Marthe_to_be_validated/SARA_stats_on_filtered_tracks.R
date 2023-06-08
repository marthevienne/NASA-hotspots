## Stat on filtered tracks

###########################################################################################################
#compute distance per day-------------------------------------------------------
rm(list=ls())

data<-read.csv("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/1_behavior_data/3_filteredTracks_Weddell_2017_modif14414.csv",head=TRUE)
data$date=as.POSIXct(data$date,origin="1970-01-01",tz="GMT")
data$dateR=as.Date(data$date)
unik=unique(data$id)

loc2 <- NULL
speedall<-NULL

dbs <- data.frame("ref"=rep(0,1) ,"distcum"=0, "speed_mean"=0,"speed_sd"=0,"speed_max"=0,"dist"=0,"sddist"=0)   

for (p in 1:length(unik)){
  distance_travelled<-NULL
  distance_cum<-NULL
  dtime<-NULL
  
  dbs2 <- data.frame("ref"=rep(0,1) ,"distcum"=0, "speed_mean"=0,"speed_sd"=0,"speed_max"=0,"dist"=0,"sddist"=0)   
  
  
  sa <- data[data$id==unik[p],]
  
  d.time <- diff(sa$date) #min
  dbs2$ref=as.character(sa$id[1])
  
  sa=sa[with(sa, order(id, date)), ]
  # dbs2$duration_diag=difftime(sa[nrow(sa),2],sa[1,2],units = c("days"))#duree diag
  # dbs2$start=sa[1,2]#debut des loc
  # dbs2$end= sa[nrow(sa),2]#fin des loc
  
  distance_cum <-c(NA,cumsum(distanceTrack(sa$lat,sa$lon)))
  dbs2$distcum<-max(na.omit(distance_cum))
  
  #data for speed--------------------------------------
  
  #dist
  sa$distance_travelled<-c(NA,distanceTrack(sa$lat,sa$lon)) 
  
  #dtime 
  t<-sa$date
  for (n in 1: (length(t)-1)){
    dtime[1]<-NA
    dtime[n+1]<-difftime(t[n+1], t[n], units="hours") 
  }
  #speed
  sa$dtime=dtime
  speed=sa$distance_travelled/sa$dtime #km/h
  rm(a)
  a=which(nchar(as.character(speed))==3)
  if (length(a)!=0){
    speed=speed[-a]}
  
  rm(b)
  b=which(speed >=6)
  if (length(b)!=0){
    speed=speed[-b]}
  
  speedall=c(speedall,speed)
  dbs2$speed_mean=mean(na.omit(speed))
  dbs2$speed_sd=sd(na.omit(speed))
  dbs2$speed_max=max(na.omit(speed))
  #end data for speed------------------------------------
  
  unic=unique(as.numeric(sa$dateR))  
  loc <- NULL
  
  for (i in unic) {
    
    dataOneDay <- sa[which(sa$dateR == i),]
    loc <- c(loc, cumsum(dataOneDay$distance_travelled))
    loc2 <- c(loc2,cumsum(dataOneDay$distance_travelled))# on garde tous les dist par jour pour faire une moy sur tous les ind
  }
  #on calcule la dist par jour
  loc=as.data.frame(loc)
  dbs2$dist=mean(na.omit(loc$loc))#
  dbs2$sddist=sd(na.omit(loc$loc))#
  dbs=rbind(dbs,dbs2)
}
dbs=dbs[-1,]

speedall <- as.data.frame(speedall)
sapply(na.omit(speedall), mean)
sapply(na.omit(speedall), sd) 

loc2=as.data.frame(loc2)# dist par jour pour tous
sapply(na.omit(loc2), mean)
sapply(na.omit(loc2), sd) 