setwd("~/Desktop/WHOI/Data/output_data/")

## Dives
dives_ctd = readRDS("dive_metrics_Vtest_ctd")

## Tracks
dives = readRDS("dive_metrics_V8")

seals = unique(dives$REF)
for (seal in seals) {
  dives_ind = subset(dives, REF == seal)
  dives_ctd_ind = subset(dives_ctd, REF == seal)
  summary_dive <- dives_ind %>% 
    group_by(day = DE_DATE) %>% 
    summarize(n_dives = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  summary_track <- dives_ctd_ind %>%
    group_by(day = DE_DATE) %>% 
    summarize(n_dives = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  rug_plot <- ggplot() +
    geom_tile(data = summary_dive, aes(x = day, y = "dives", col = n_dives))+
    geom_tile(data = summary_track, aes(x = day, y = "CTD", col = n_dives))+
    xlab("") +
    ylab("") +
    theme_light()+
    theme(legend.position = "none")
  
  file_output = sprintf("~/Dropbox/data/outputs_Marthe_2023/rug_ctd_dives/%s.png", seal)
  ggsave(plot = rug_plot, filename = file_output, height = 4, width = 15, units = "cm", dpi = 150)
}

