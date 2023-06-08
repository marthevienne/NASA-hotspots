## Stats dives 2

# Stat--------------------------------------------------------------------------

dives_num = read.table(
  "C:/Users/lucie/Dropbox/codes/dives_num_SES_polynya/filtered_dive_ses_polynya_5meters_bathy_hunt_bentpela_NumPolMult_shelf_BehaInd_MNDepHT.txt",
  sep = ",",
  head = T
)

unik = unique(dives_num$id)

dives_num$date_fin = as.POSIXct(dives_num$date_fin, origin = "1970-01-01", tz = "GMT")
dives_num$dateR = as.Date(dives_num$date_fin)

loc2 <- NULL
md <- NULL
mdur = NULL

dbs <-
  dives_num.frame(
    "ref" = rep(0, 1) ,
    "m_depth" = 0,
    "sd_depth" = 0,
    "m_dur" = 0,
    "sd_dur" = 0,
    "nb_loc" = 0,
    "sd_loc" = 0,
    "max_depth" = 0,
    "min_depth" = 0
  )

for (p in 1:length(unik)) {
  print(p)
  dbs2 <-
    dives_num.frame(
      "ref" = rep(0, 1) ,
      "m_depth" = 0,
      "sd_depth" = 0,
      "m_dur" = 0,
      "sd_dur" = 0,
      "nb_loc" = 0,
      "sd_loc" = 0,
      "max_depth" = 0,
      "min_depth" = 0
    )

  sa <- dives_num[dives_num$id == unik[p],]

  dbs2$max_depth <-
    max(sa$max_depth) # profondeur plong?e la plus profonde
  dbs2$min_depth <-
    min(sa$max_depth) # profondeur plong?e la moins profonde
  dbs2$m_depth <-
    mean(sa$max_dep)#  moyenne de temps entre les locs en hours
  dbs2$sd_depth <- sd(sa$max_dep)# ecart-types en hours
  md <-
    c(md, sa$max_dep) # on garde tous les écarts de temps pour faire une moy sur tous les ind, en sec
  dbs2$ref = as.character(sa$id[1])

  dbs2$m_dur = mean(sa$dive_dur) / 60
  dbs2$sd_dur = sd(sa$dive_dur) / 60
  mdur <- c(mdur, sa$dive_dur)

  unic = unique(as.numeric(sa$dateR))
  loc <- dives_num.frame()

  for (i in unic) {
    dives_numOneDay <- sa[which(sa$dateR == i),]
    locperDay <- length(dives_numOneDay$startlat)
    loc <- rbind(loc, locperDay)
    loc2 <- c(loc2, locperDay)
  }

  dbs2$nb_loc = sapply(loc, mean)
  dbs2$sd_loc = sapply(loc, sd)
  dbs = rbind(dbs, dbs2)
}
dbs = dbs[-1,]

# Save dbs as a .csv file in Dropbox -> dives non r?interpol?es avec les tracks filtr?s
write.csv(dbs, file = "C:/Users/lucie/Dropbox/codes/dives_num_SES_polynya/stat_dive_outliers_removed_boundary_non_interpoled_loc_5meters.csv", row.names =
            T)

md <- as.dives_num.frame(md)#Profondeur max des dives
(sapply(md, mean))
(sapply(md, sd))

md_dur <- as.dives_num.frame(mdur)#Dur?e des dives
(sapply(md_dur, mean) / 60)
(sapply(md_dur, sd) / 60)

loc2 = as.dives_num.frame(loc2)# nb de locs par jour pour tous
sapply(loc2, mean)
sapply(loc2, sd)

# Stats pour nb de plong?es dans une polynie et nb de plong?es total

dbs3 <- dives_num.frame(
  "ref" = rep(0, 1),
  "nb_dives_pol" = 0 ,
  "nb_dives" = 0
)

for (seal in 1:length(unique(dives_num$id))) {
  dives_num_ind <-
    dives_num[dives_num$id == unique(dives_num$id)[seal],] # selectionner dives de l'individu
  dbs4 <-
    dives_num.frame(
      "ref" = rep(0, 1),
      "nb_dives_pol" = 0 ,
      "nb_dives" = 0
    ) # tableau vide pour cet individu

  dbs4$ref = as.character(dives_num_ind$id[1])
  dbs4$nb_dives_pol = length(which(dives_num_ind$pol != 0))
  dbs4$nb_dives = dim(dives_num_ind)[1]

  dbs3 = rbind(dbs3, dbs4)
}
dbs3 = dbs3[-1,]

write.csv(dbs3, file = "C:/Users/lucie/Dropbox/codes/dives_num_SES_polynya/statNumPol_dive_outliers_removed_boundary_non_interpoled_loc_5meters.csv", row.names =
            T)


################################################################################
# Apres avoir fait tourner le BSSM on reinterpole les locs,

rm(list = ls())

dive <-
  read.table(
    "/Users/lucie/Documents/Master_MODE_Rennes/M2/Stage/Documents_StageM2/dives_num_Lucie/all_dive_outliers_removed_SES_polynia_boundary_newdives_num_5meters.txt",
    sep = ",",
    head = T
  )
dive$date_fin <-
  as.POSIXct(dive$date_fin, origin = "1970-01-01", tz = "GMT")
str(dive)

BSSM = read.csv(
  "C:/Users/lucie/Dropbox/codes/ssm/filteredTracks_ssm_polynya_updated.csv",
  stringsAsFactors = F
)
BSSM$date <-
  as.POSIXct(BSSM$date, origin = "1970-01-01", tz = "GMT")
str(BSSM)

#----------------------------------reinterpole les locs dive ac le bssm
dt1 = NULL
seal = unique(BSSM$id)

#we remove the part where the bssm did not work and keep the raw dives_num

# a=which(dive$id=="wd09-414-16" & as.numeric(format(dive$date,"%m"))>=6)
# dt2=dive[a,]
# dive=dive[-a,]

for (p in 1:length(seal)) {
  print(p)
  sa <- dive[dive$id == seal[p],]
  bssm <- BSSM[BSSM$id == seal[p],]

  interp1 <- approx(bssm$date, bssm$lon, xout = sa$date_fin)
  sa$lon <- interp1$y
  interp2 <- approx(bssm$date, bssm$lat, xout = sa$date_fin)
  sa$lat <- interp2$y

  dt1 <- rbind(dt1, sa)
}

# dt2$lon=dt2$startlon
# dt2$lat=dt2$startlat
#
# all=rbind(dt1,dt2)

write.table(dt1,
            file = "C:/Users/lucie/Dropbox/codes/filtered_dive_ses_polynya_5meters.txt",
            sep = ",",
            row.names = F)