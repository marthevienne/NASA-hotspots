#2------------------------------------------------------------------------------
#Run BSSM-----------------------------------------------------------------------
#-------------------------------------------------------------------------------
rm(list=ls())

## Working directory
setwd("~/Desktop/WHOI/WD_Data_codes_Marthe")

## Library
install.packages("aniMotum", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE)
library(aniMotum) # careful about R version compatibility
library(ggplot2)
library(tibble)

## Source a modified version of Ian Jonsen's quality control plot function
## original at:
## https://github.com/SCAR/RAATD/blob/master/R/duckConfit/R/qc_plot.r
# source("/Users/saralabrousse/Dropbox/All_PHD_2/PHD/Projet_Weddell_Sea_2017/Seal_analysis_oceano/codes/plotFun.R")


## Read data
data <- read.csv("Data/bssm_SES_polynia.csv", stringsAsFactors = F)
data$date=as.POSIXct(data$date,origin="1970-01-01",tz="GMT")
str(data)


## Fit the SSM individual by individual

foo <- data.frame()
foo2 <- data.frame()

for (i in 1:length(unique(data$id))) {
  
  ## Individual i
  this.id <- unique(data$id)[i]
  print(this.id)
  print(paste0(ceiling(i/length(unique(data$id)) * 100), " %"))
  
  ## Subset by individual
  d <- data[data$id == this.id, ]
  
  ## Check track duration
  dur <- difftime(max(d$date), min(d$date), units = "hours")
  
  ## Fit the SSM
  fit <- fit_ssm(d, time.step = 4, model = "crw", vmax = 4, control = ssm_control(verbose = 0)) # à comprendre
  fmp <- fit_mpm(fit, what = "fitted", model = "mpm", control = mpm_control(verbose = 0)) # à comprendre 
  
  ## Ian's plots
  pdf(paste0("Figures/",
             sprintf("id_%s_2.pdf",this.id)),height=10,width=8)
  
  #a ggplot object with either: (type = 1) 1-d time series of fits to data, separated into x and y components
  #(units = km) with prediction uncertainty ribbons (2 x SE); or (type = 2) 2-d fits to data (units = km)
  
  print(plot(
    fit,
    what = c("predicted"), type = 1,
    #1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
    outlier = TRUE,
    pages = 1, #plots of all individuals on a single page (pages = 1; default) or each individual
    #on a separate page (pages = 0)
    ncol = 1,
    ask=FALSE
  )) #jaune- outlier location dropped by prefilter
  
  print(plot(
    fit,
    what = c("predicted"), type = 2,
    #1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
    outlier = TRUE,
    pages = 1, #plots of all individuals on a single page (pages = 1; default) or each individual
    #on a separate page (pages = 0)
    ncol = 1,
    ask=FALSE
  )) #jaune- outlier location dropped by prefilter

  #print(fmap(fit, what = "predicted", obs = TRUE, size = c(0.5, 0.8))) #when mapping single tracks, should locations be coloured by date (logical; default = FALSE)
  print(plot(fmp, pal = "Cividis", ask=FALSE))
  
  ## using cowplot to add southern elephant seal silhouettes to map
  #print(ggdraw() +
  #        draw_plot(m))
  #+draw_image("Figures/sese_female_orig.png",  x=0.85, y=0.45, scale=0.25, hjust=0.5, vjust=0.5))
  
  print(map(fit, what = "predicted", crs = "+proj=stere +lon_0=69 +units=km +datum=WGS84"))

  dev.off()
  
  
  ## Get the predicted locations
  this.fit <- grab(fit, what = "predicted", as_sf = FALSE)
  this.fit2 <- grab(fmp,  as_sf = FALSE)

  foo <- rbind(foo, this.fit)
  foo2 <-rbind(foo2, this.fit2)
}


# Multi tracks
## Fit the SSM
d <- as_tibble(data)
fit <- fit_ssm(d, time.step = 4, model = "crw", vmax = 4, control = ssm_control(verbose = 0)) # à comprendre
fmp <- fit_mpm(fit, what = "fitted", model = "mpm", control = mpm_control(verbose = 0)) # à comprendre 

map(fit,  what = "predicted", crs = "+proj=stere +lon_0=69 +units=km +datum=WGS84") + 
  theme(legend.position="none")

my.aes = aes_lst(id_pal = hcl.colors(n=226, palette = "RdBu"))
map(fit,  what = "predicted", aes = my.aes) + 
  theme(legend.position="none")

df = data.frame(x, y)
x = rnorm(226)
y = rnorm(226)

ggplot(df, aes(x = x, y = y, color = as.factor(x))) +
  geom_point() + 
  scale_color_manual(values = my.pal)

my.pal = hcl.colors(n=226, palette = "RdBu")

## Track plots
ggplot(data = foo, aes(x = lon, y = lat, colour = id)) +
  geom_point(size = .4, show.legend = FALSE) +
  coord_quickmap()


## Write
write.csv(foo, "Data/filteredTracks_ssm_polynya_updated.csv", row.names = F)
write.csv(foo2, "Data/filteredTracks_ssm_polynya_behavior_updated.csv", row.names = F)
save(fit, file = "Data/fitted_tracks")

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



