#7_calcul hunting time et definition benthic/pelagic----------------------------
setwd("~/Desktop/WHOI/Data/")

rm(list=ls())

dives = readRDS("filtered_dives_north_boundary_5meters_bathy") # pour l'instant
dives$DE_DATE = as.POSIXct(dives$DE_DATE,origin="1970-01-01",tz="GMT")

#1. Etudes des plongees plus profondes que la bathy ---------------------------
r = dives
r <- r[which(r$bathy!=-9999),]
r$Bathy <- abs(r$bathy)
r$above_depth <- r$Bathy-r$MAX_DEP
ab <- r$above_depth[r$above_depth < 0]

#116/2903=3,9%

a=which(r$above_depth < 0)
dt=r[a,]

library(ggplot2)
ggplot(data=dt, mapping = aes(x=above_depth, y=bathy, color=REF)) +
  theme_bw() +
  geom_point(size=2)+
  theme(legend.position = "none")

ggplot(data=dt, mapping=aes(x=START_LON, y=START_LAT, color=REF)) +
  theme_bw() +
  geom_point(size=2) + 
  theme(legend.position = "none")

ggplot(data=dt, mapping=aes(x=START_LON, y=START_LAT, color=above_depth)) +
  theme_bw() +
  geom_point(size=2)+
  #coord_map()+
  labs(x="Longitude (?E)", y="Latitude (?N)",colour="Depth difference (m)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12,face="bold"),
        legend.title=element_text(size=12,face="bold"))


#2 calcul du mode-------------------------------------------------------------- 
tiff(paste("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/plots/hist_dive_above_bathy.tiff"),res=700,height=14,width=24,units="cm",compression="lzw")
hist(r$above_depth,freq=T,breaks=200, xlim=c(-800,5000), main="",xlab="Difference between bathymetry and maximum depth of the dive")

abline(v=70, col="red")
abline(v=50, col="black")
abline(v=70, col="green")
abline(v=100, col="blue")
dev.off()



#dive deeper set to zero
a=which(r$above_depth < 0)
dt=r[a,]
r$above_depth[a]=0

#2. Selection des dives ---------------------------------------------------------------------------
r$dive_type <- NA

r$dive_type[r$above_depth > 70] <- "pelagic"
r$dive_type[r$above_depth <= 70] <- "benthic"

#4. Plot tout trajet et couleur identification dives ----------------------------------------------

unik=unique(r$id)

library(ggplot2)
library(fields)
library(scales)
#r$bathy[r$bathy > 2000]=NA
S <- split(r,r$REF)

#for (i in 1:4){
  i=i+1
  #dt=S[[i]]
  #tiff(paste("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/plots/",S[[i]]$id[1],"dive_type.tiff"),res=700,height=12,width=28,units="cm",compression="lzw")
  
  S[[i]]$T0=as.POSIXct(S[[i]]$T0,origin="1970-01-01",tz="GMT")
  S[[i]]$T1=as.POSIXct(S[[i]]$T1,origin="1970-01-01",tz="GMT")
  S[[i]]$T2=as.POSIXct(S[[i]]$T2,origin="1970-01-01",tz="GMT")
  S[[i]]$T3=as.POSIXct(S[[i]]$T3,origin="1970-01-01",tz="GMT")
  S[[i]]$T4=as.POSIXct(S[[i]]$T4,origin="1970-01-01",tz="GMT")
  S[[i]]$Tf=as.POSIXct(S[[i]]$Tf,origin="1970-01-01",tz="GMT")
  
  dep <- c(S[[i]]$D0,S[[i]]$D1,S[[i]]$D2,S[[i]]$D3,S[[i]]$D4,S[[i]]$Df)
  tps <-c(S[[i]]$T0,S[[i]]$T1,S[[i]]$T2,S[[i]]$T3,S[[i]]$T4,S[[i]]$Tf)
  cl <- c(S[[i]]$dive_type,S[[i]]$dive_type,S[[i]]$dive_type,S[[i]]$dive_type,S[[i]]$dive_type,S[[i]]$dive_type)
  data=as.data.frame(cbind(dep,tps,cl))
  data$tps=as.POSIXct(as.character(tps),origin="1970-01-01",tz="GMT")
  data$dep=as.numeric(as.character(dep))
  obs_benthic=nrow(S[[i]][S[[i]]$dive_type=="benthic",])/nrow(S[[i]])
  
  ggplot(data=data,aes(x=tps, y=-dep,color=cl)) +
    theme_bw() +
    geom_line(size=0.4) +
    geom_point(data = S[[i]], aes(x=T0, y=-Bathy), size=0.4, color="black")+
    scale_x_datetime(labels = date_format("%m"))+
    #scale_x_date(labels = date_format("%m"))+
    labs(x="Time (month) ", y="Depth (m)", colour="Dive strategy",size=6)+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"))+
    ggtitle(paste("Percentage Benthic dives",round(obs_benthic,digits=2)))
  
  dev.off()
#}

a=which(r$dive_type=="benthic") #186637 dives
b=which(r$dive_type=="pelagic") #408382 dives

#186637/595019 = 0.3136656

#map of benthic versus pelagic--------------------------------------------------
library(raster)
library(trip)
library(fields)
library(rworldxtra)
library(graticule)
data(countriesHigh)
library(ggplot2)
library(marmap)
library(scales)

#topo-
topo <- raster("~/Dropbox/data/bathymetry/GEBCO_03_Feb_2022_cc82bde5d257/gebco_2021_n-58.0_s-74.0_w-2.0_e160.0.tif")
#xylim=c(-55, -35, -80,-75)
topo[topo > -1] <- NA
#plot(topo)
# extopo <- crop(topo, extent(-37.5, -34, -77.8,-77.2)) 
extopo <- crop(topo, extent(-2, -25, -80,-70.5)) 
topo
extopo2=as.bathy(topo)

r$dive_type[r$above_depth > 50 ] <- "1_pelagic"
r$dive_type[r$above_depth <=50] <- "2_demersal"

p1<-autoplot(topo, geom=c("raster","contour"))+
  geom_point(data = as.data.frame(r), aes(START_LON, START_LAT,  colour = dive_type, fill = NULL),size=1.5,alpha=0.5) + 
  labs(x="Longitude", y="Latitude",fill="Topography",colour="Dive type")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=9,face="bold"),
        legend.title=element_text(size=16,face="bold"))+
  scale_colour_manual(values = c( "green","red"))


tiff("test.tiff",height=900,width=600,units="px")
p1
dev.off()

write.table(r, file="C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela.txt",sep=",",row.names=F)

#calcul du hunting time---------------------------------------------------------
r$dive_type[r$above_depth > 50 ] <- 1
r$dive_type[r$above_depth <=50] <- 2

dt=r

dt$coef1=(dt$D1-dt$D0)/(dt$T1-dt$T0)
dt$coef2=(dt$D2-dt$D1)/(dt$T2-dt$T1)
dt$coef3=(dt$D3-dt$D2)/(dt$T3-dt$T2)
dt$coef4=(dt$D4-dt$D3)/(dt$T4-dt$T3)
dt$coef5=(dt$Df-dt$D4)/(dt$Tf-dt$T4)

dt$tim1=(dt$T1-dt$T0)
dt$tim2=(dt$T2-dt$T1)
dt$tim3=(dt$T3-dt$T2)
dt$tim4=(dt$T4-dt$T3)
dt$tim5=(dt$Tf-dt$T4)

dt$k1[abs(dt$coef1) <= 0.5] <-1
dt$k1[abs(dt$coef1) > 0.5] <- 0

dt$k2[abs(dt$coef2) <= 0.5] <-1
dt$k2[abs(dt$coef2) > 0.5] <- 0

dt$k3[abs(dt$coef3) <= 0.5] <-1
dt$k3[abs(dt$coef3) > 0.5] <- 0

dt$k4[abs(dt$coef4) <= 0.5] <-1
dt$k4[abs(dt$coef4) > 0.5] <- 0

dt$k5[abs(dt$coef5) <= 0.5] <-1
dt$k5[abs(dt$coef5) > 0.5] <- 0

for (i in 1:nrow(dt)){
  print(i)
  dt2=dt[i,]
  dt$hunting[i]=sum(dt2$tim2[dt2$k2==1],dt2$tim3[dt2$k3==1],dt2$tim4[dt2$k4==1])/60
}

#save here the data-------------------------------------------------------------

#write.table(dt, file="C:/Users/lucie/Dropbox/codes/data_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt.txt",sep=",",row.names=F)

#stat here----------------------------------------------------------------------
rm(list=ls())
data=read.table("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/1_behavior_data/5_filtered_dive_weddell_2017_modif14414_ht_benthicpelagic.txt",sep=",",head=T)

# a=which(data$dive_type==1) #pelagic
# #2393/2903=82%
# a=which(data$dive_type==2)

unik=unique(data$id)

data$date_fin=as.POSIXct(data$date_fin,origin="1970-01-01",tz="GMT")
data$dateR=as.Date(data$date_fin)

mdpel <- NULL
mdbent <-NULL
mdhtpel<-NULL
mdhtbent<-NULL

dbs <- data.frame("ref"=rep(0,1) ,"m_depth_pel"=0,"sd_depth_pel"=0, "m_depth_bent"=0, "sd_depth_bent"=0,
                  "ht_pel"=0,"sd_ht_pel"=0,"ht_bent"=0,"sd_ht_bent"=0)   

for (p in 1:length(unik)){
 
  dbs2 <- data.frame("ref"=rep(0,1) ,"m_depth_pel"=0,"sd_depth_pel"=0, "m_depth_bent"=0, "sd_depth_bent"=0,
                    "ht_pel"=0,"sd_ht_pel"=0,"ht_bent"=0,"sd_ht_bent"=0)   
  
  sa <- data[data$id==unik[p],]
  print(sa$id[1])
  a=which(sa$dive_type==1)
  print((length(a)/nrow(sa)))
  
  dbs2$m_depth_pel <- mean(sa$max_dep[sa$dive_type==1])
  dbs2$sd_depth_pel <- sd(sa$max_dep[sa$dive_type==1])
  mdpel <- c(mdpel,sa$max_dep[sa$dive_type==1]) 
  
  dbs2$m_depth_bent <- mean(sa$max_dep[sa$dive_type==2])
  dbs2$sd_depth_bent <- sd(sa$max_dep[sa$dive_type==2])
  mdbent <- c(mdbent,sa$max_dep[sa$dive_type==2]) 
  
  dbs2$ref=as.character(sa$id[1])
  
  dbs2$ht_pel <- mean(sa$hunting[sa$dive_type==1])
  dbs2$sd_ht_pel <- sd(sa$hunting[sa$dive_type==1])
  mdhtpel <- c(mdhtpel,sa$hunting[sa$dive_type==1]) 
  
  dbs2$ht_bent <- mean(sa$hunting[sa$dive_type==2])
  dbs2$sd_ht_bent <- sd(sa$hunting[sa$dive_type==2])
  mdhtbent <- c(mdhtbent,sa$hunting[sa$dive_type==2]) 
  
  dbs <-rbind(dbs,dbs2)
}
dbs=dbs[-1,]

mdpel <- as.data.frame(mdpel)
(sapply(mdpel, mean)) 
(sapply(mdpel, sd)) 

mdbent <- as.data.frame(mdbent)
(sapply(mdbent, mean)) 
(sapply(mdbent, sd))

mdhtpel <- as.data.frame(mdhtpel)
(sapply(mdhtpel, mean)) 
(sapply(mdhtpel, sd)) 

mdhtbent <- as.data.frame(mdhtbent)
(sapply(mdhtbent, mean)) 
(sapply(mdhtbent, sd))


#plot---------------------------------------------------------------------------
#-------------------------------------------------------------------------------
dt$tim1[dt$k1==0]=0
dt$tim2[dt$k2==0]=0
dt$tim3[dt$k3==0]=0
dt$tim4[dt$k4==0]=0
dt$tim5[dt$k5==0]=0

#based on Phtopoulou et al. (2020) - In contrast to [1] we do not consider the first and last segments 
#(initial descent and final ascent) of each dive. We do this because Weddell seals likely have to travel 
#some distance horizontally under the ice from and to their breathing holes and this may produce shallow-angle 
#swimming unrelated to foraging. The findings of [2] also support the exclusion of “commuting” segments from being 
#important for foraging. This post hoc analysis of the PrCA data from [1] shows that only a small percentage of PrCA 
#behaviours occur in the first and last segments (6%), which is further evidence that we are unlikely to be excluding 
#highly important foraging behaviour by removing them from our analysis.
dt$tim1[dt$k1==1]=0
dt$tim5[dt$k5==1]=0

#restrict for plotting
quantile(dt$hunting)
#dt$hunting[dt$hunting>8]=8

for (i in 1:nrow(dt)){

dt$httime1[i]=0
dt$httime2[i]=0
dt$httime3[i]=0
dt$httime4[i]=0
dt$httime5[i]=0
# if(dt$tim2[i] >dt$tim3[i] & dt$tim2 [i] >dt$tim4[i]){
# dt$httime2[i]=mean(dt$D1[i],dt$D2[i])}else{dt$httime2[i]=0}
# 
# if(dt$tim3[i] >dt$tim2[i] & dt$tim3 [i] >dt$tim4[i]){
#   dt$httime3[i]=mean(dt$D2[i],dt$D3[i])}else{dt$httime3[i]=0}
# 
# if(dt$tim4[i] >dt$tim2[i] & dt$tim4 [i] >dt$tim3[i]){
#   dt$httime4[i]=mean(dt$D3[i],dt$D4[i])}else{dt$httime4[i]=0}

if(dt$tim2[i] > dt$tim3[i] & dt$tim2[i] >dt$tim4[i]){
  dt$httime2[i]=dt$tim2[i]/60}

if(dt$tim3[i] >dt$tim2[i] & dt$tim3[i] >dt$tim4[i]){
  dt$httime3[i]=dt$tim3[i]/60}

if(dt$tim4[i] > dt$tim2[i] & dt$tim4[i] >dt$tim3[i]){
  dt$httime4[i]=dt$tim4[i]/60}
}

#-------------------------------------------------------------------------------
a=which(dt$bathy < -1500)
dt$bathy[a]=NA

S <- split(dt,dt$id)

S[[i]]$T0=as.POSIXct(S[[i]]$T0,origin="1970-01-01",tz="GMT")
S[[i]]$T1=as.POSIXct(S[[i]]$T1,origin="1970-01-01",tz="GMT")
S[[i]]$T2=as.POSIXct(S[[i]]$T2,origin="1970-01-01",tz="GMT")
S[[i]]$T3=as.POSIXct(S[[i]]$T3,origin="1970-01-01",tz="GMT")
S[[i]]$T4=as.POSIXct(S[[i]]$T4,origin="1970-01-01",tz="GMT")
S[[i]]$Tf=as.POSIXct(S[[i]]$Tf,origin="1970-01-01",tz="GMT")

dep <- c(S[[i]]$D0,S[[i]]$D1,S[[i]]$D2,S[[i]]$D3,S[[i]]$D4,S[[i]]$Df)
tps <-c(S[[i]]$T0,S[[i]]$T1,S[[i]]$T2,S[[i]]$T3,S[[i]]$T4,S[[i]]$Tf)
#cl <- c(S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting)
cl<-c(rep(0,nrow(S[[i]])),S[[i]]$httime1,S[[i]]$httime2,S[[i]]$httime3,S[[i]]$httime4,S[[i]]$httime5)
data=as.data.frame(cbind(dep,tps,cl))
data$tps=as.POSIXct(as.character(tps),origin="1970-01-01",tz="GMT")
data$dep=as.numeric(as.character(dep))
obs_benthic=nrow(S[[i]][S[[i]]$dive_type==2,])/nrow(S[[i]])

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

p1<-ggplot(data=data,aes(x=tps, y=-dep,color=cl)) +
  theme_bw() +
  geom_line(size=0.4) +
  geom_point(data = S[[i]], aes(x=T0, y=bathy), size=0.4, color="black")+
  scale_x_datetime(labels = date_format("%m"))+
  #scale_x_date(labels = date_format("%m"))+
  labs(x="Time (month) ", y="Depth (m)", colour="Hunting time per seg. (min)",size=6)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
  ggtitle(paste("Percentage demersal dives",round(obs_benthic,digits=2)))+
  scale_color_gradient(low="green",high="red",limits=c(0,8))

S[[i]]$T0=as.POSIXct(S[[i]]$T0,origin="1970-01-01",tz="GMT")
S[[i]]$T1=as.POSIXct(S[[i]]$T1,origin="1970-01-01",tz="GMT")
S[[i]]$T2=as.POSIXct(S[[i]]$T2,origin="1970-01-01",tz="GMT")
S[[i]]$T3=as.POSIXct(S[[i]]$T3,origin="1970-01-01",tz="GMT")
S[[i]]$T4=as.POSIXct(S[[i]]$T4,origin="1970-01-01",tz="GMT")
S[[i]]$Tf=as.POSIXct(S[[i]]$Tf,origin="1970-01-01",tz="GMT")

dep <- c(S[[i]]$D0,S[[i]]$D1,S[[i]]$D2,S[[i]]$D3,S[[i]]$D4,S[[i]]$Df)
tps <-c(S[[i]]$T0,S[[i]]$T1,S[[i]]$T2,S[[i]]$T3,S[[i]]$T4,S[[i]]$Tf)
cl <- c(S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting,S[[i]]$hunting)
#cl<-c(rep(0,nrow(S[[i]])),S[[i]]$httime1,S[[i]]$httime2,S[[i]]$httime3,S[[i]]$httime4,S[[i]]$httime5)
data=as.data.frame(cbind(dep,tps,cl))
data$tps=as.POSIXct(as.character(tps),origin="1970-01-01",tz="GMT")
data$dep=as.numeric(as.character(dep))
obs_benthic=nrow(S[[i]][S[[i]]$dive_type==2,])/nrow(S[[i]])

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

p2<-ggplot(data=data,aes(x=tps, y=-dep,color=cl)) +
  theme_bw() +
  geom_line(size=0.4) +
  geom_point(data = S[[i]], aes(x=T0, y=bathy), size=0.4, color="black")+
  scale_x_datetime(labels = date_format("%m"))+
  #scale_x_date(labels = date_format("%m"))+
  labs(x="Time (month) ", y="Depth (m)", colour="Hunting time per dive (min)",size=6)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
  ggtitle(paste("Percentage demersal dives",round(obs_benthic,digits=2)))+
  scale_color_gradient(low="green",high="red",limits=c(0,12))

tiff(paste("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/plots/",S[[i]]$id[1],"_hunting.tiff"),height=1000,width=800,units="px")
grid.arrange(p1,p2,heights = c(3/6,3/6))
dev.off()


# #just attempt 1
# S <- split(dt,dt$id)
# S[[i]]$T0=as.POSIXct(S[[i]]$T0,origin="1970-01-01",tz="GMT")
# S[[i]]$T1=as.POSIXct(S[[i]]$T1,origin="1970-01-01",tz="GMT")
# S[[i]]$T2=as.POSIXct(S[[i]]$T2,origin="1970-01-01",tz="GMT")
# S[[i]]$T3=as.POSIXct(S[[i]]$T3,origin="1970-01-01",tz="GMT")
# S[[i]]$T4=as.POSIXct(S[[i]]$T4,origin="1970-01-01",tz="GMT")
# S[[i]]$Tf=as.POSIXct(S[[i]]$Tf,origin="1970-01-01",tz="GMT")
# 
# dep <- c(S[[i]]$D0,S[[i]]$D1,S[[i]]$D2,S[[i]]$D3,S[[i]]$D4,S[[i]]$Df)
# tps <-c(S[[i]]$T0,S[[i]]$T1,S[[i]]$T2,S[[i]]$T3,S[[i]]$T4,S[[i]]$Tf)
# cl <- c(rep(0,nrow(S[[i]])),S[[i]]$tim1/60,S[[i]]$tim2/60,S[[i]]$tim3/60,S[[i]]$tim4/60,S[[i]]$tim5/60)
# data=as.data.frame(cbind(dep,tps,cl))
# data$tps=as.POSIXct(as.character(tps),origin="1970-01-01",tz="GMT")
# data$dep=as.numeric(as.character(dep))
# obs_benthic=nrow(S[[i]][S[[i]]$dive_type=="benthic",])/nrow(S[[i]])
# 
# jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# 
# ggplot(data=data,aes(x=tps, y=-dep,color=cl)) +
#   theme_bw() +
#   geom_line(size=0.4) +
#   geom_point(data = S[[i]], aes(x=T0, y=-Bathy), size=0.4, color="black")+
#   scale_x_datetime(labels = date_format("%m"))+
#   #scale_x_date(labels = date_format("%m"))+
#   labs(x="Time (month) ", y="Depth (m)", colour="Dive strategy",size=6)+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12,face="bold"))+
#   ggtitle(paste("Percentage Benthic dives",round(obs_benthic,digits=2)))+
#   scale_color_gradientn(colours=jet.colors(7))

