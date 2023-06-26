## Get diverse info on seal
setwd("~/Desktop/WHOI/Data/output_data/")

seal = "ct170-707-22"

## Track and dive rug
dives = readRDS("filtered_numbered_dives")
tracks = readRDS("tracks_for_bssm")
bssm = readRDS("predictedTracks_ssm")

dives_ind = subset(dives, REF == seal)
track_ind = subset(tracks, id == seal)
bssm_ind = subset(bssm, id == seal)

dive_QI <- as.data.frame(dives_ind$NUM)
dive_QI$x.se = unlist(sapply(dives_ind$DE_DATE, function(x) bssm_ind$x.se[which(abs(x - bssm_ind$date) == min(abs(x - bssm_ind$date)))[1]])) 
dive_QI$y.se = unlist(sapply(dives_ind$DE_DATE, function(x) bssm_ind$y.se[which(abs(x - bssm_ind$date) == min(abs(x - bssm_ind$date)))[1]])) 

plot(dive_QI$x.se, pch = 16, cex = .3)
plot(dive_QI$y.se)
hist(dive_QI$y.se, pch = 16, cex = .3, breaks = 100, xlim = c(0, 50))

threshold = 10 # km

toto = subset(dive_QI, x.se < threshold & y.se < threshold)
plot(toto$x.se, pch = 16, cex = .3)
nrow(toto)
nrow(dive_QI)

summary_dive <- dives_ind %>% 
  group_by(day = DE_DATE) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()

summary_track <- track_ind %>%
  group_by(day = date) %>% 
  summarize(n_loc = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_loc/sum(n_loc)) %>%
  as.data.frame()

ggplot() +
  geom_tile(data = summary_dive, aes(x = day, y = "dives", col = n_dives))+
  geom_tile(data = summary_track, aes(x = day, y = "track", col = n_loc))+
  xlab("")+
  ylab("")+
  theme_light()#+
  theme(legend.position = "none")
  
## Dives plot
dives_table = readRDS("output_data/dives_table")
d_profiles = dives_table[which(dives_table$REF == seal), ]
# d_profiles$depth[which(d_profiles$depth == 0)] = 5

ggplot(d_profiles, aes(x = as_datetime(time), y = depth)) +
  geom_line(linewidth = .1) + 
  scale_y_reverse() +
  labs(x="Time", y="Depth") +
  ggtitle(seal) +
  theme_bw() +
  theme(axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=17,face="bold"), #__________ legend texts
        legend.title=element_text(size=17,face="bold"))

## Plot one dive
num = 1

d_profile = subset(dives_table, REF == seal & NUM == num)

ggplot(d_profile, aes(x = as_datetime(time), y = depth)) +
  geom_point(size = 3) + 
  geom_line(linewidth = .2) +
  scale_y_reverse() +
  labs(x="Time", y="Depth") +
  ggtitle(seal) +
  theme_bw() +
  theme(axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=17,face="bold"), #__________ legend texts
        legend.title=element_text(size=17,face="bold"))

## Compute number dives


## Summary