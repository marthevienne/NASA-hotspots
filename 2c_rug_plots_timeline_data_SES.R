setwd("~/Desktop/WHOI/Data/")

## Dives
dives = readRDS("behavioural_data/compiled_non_filtered_dives")

## Tracks
tracks = readRDS("bssm/tracks_for_bssm")
predTracks = readRDS("bssm/predictedTracks_ssm")

## Sample SES
ind = "ct164-482-21"
dives_ind = subset(dives, REF == ind)
track_ind = subset(tracks, id == ind)
predTrack_ind = subset(predTracks, id == ind)

plot(track_ind$date, track_ind$lat, pch = 16, cex = .3)
points(predTrack_ind$date, predTrack_ind$lat, pch = 16, cex = .2, col = "red")

plot(track_ind$date, track_ind$lat)
points(dives_ind$DE_DATE, dives_ind$LAT, col = "red", pch = 16, cex = .3)


source("~/Desktop/WHOI/Codes/my_SSM_filter.R")
toto = my_SSM_filter(track_ind, pred_track_ind, 10)

# plot(toto$bssm$date, toto$bssm$x.se)
plot(predTrack_ind$date, predTrack_ind$lat, pch = 4, col = "red", cex = 1)
points(toto$bssm$date, toto$bssm$lat, pch = 16, cex = .3)


BSSM = predTracks
sa <- dives[dives$REF == ind,]
bssm <- BSSM[BSSM$id == ind,]
interpLat <- approx(bssm$date, bssm$lat, xout = sa$DE_DATE)
interpLon <- approx(bssm$date, bssm$lon, xout = sa$DE_DATE)
plot(sa$DE_DATE, sa$LAT)
plot(interpLon$y, interpLat$y, pch = 16, cex = .2)

seals = unique(tracks$id)
for (seal in seals) {
  dives_ind = subset(dives, REF == seal)
  track_ind = subset(tracks, id == seal)
  summary_dive <- dives_ind %>% 
    group_by(day = DE_DATE) %>% 
    summarize(n_dives = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  summary_track <- track_ind %>%
    group_by(day = date) %>% 
    summarize(n_loc = n()) %>% 
    na.omit() %>% 
    as.data.frame()

  rug_plot <- ggplot() +
    geom_tile(data = summary_dive, aes(x = day, y = "dives", col = n_dives))+
    geom_tile(data = summary_track, aes(x = day, y = "track", col = n_loc))+
    xlab("")+
    ylab("")+
    theme_light()+
    theme(legend.position = "none")
  
  file_output = sprintf("~/Dropbox/data/outputs_Marthe_2023/rug_track_dives/%s.png", seal)
  ggsave(plot = rug_plot, filename = file_output, height = 4, width = 15, units = "cm", dpi = 150)
}

