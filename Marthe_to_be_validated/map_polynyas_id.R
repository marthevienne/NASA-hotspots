rm(list=ls())

setwd("~/Desktop/WHOI/Data/")

path_output = "~/Dropbox/data/outputs_Marthe_2023/"

source("~/Desktop/WHOI/Codes/palette_polynya.R")

CESM_df = readRDS("df_polynyas_id_CESM")

antarctica <- ne_countries(scale = "large", returnclass = "sf", continent = "Antarctica")

antarctica_map <- antarctica %>% 
  dplyr::group_by(continent) %>% 
  dplyr::select(continent)

dives = readRDS("interpoled_locations_dives_north_boundary_5meters")

map_polynya_id_CESM <- ggplot()+
  geom_sf(data = antarctica_map, 
          aes(geometry = geometry), 
          inherit.aes = F, 
          fill = "grey")+
  coord_sf(xlim = c(-5, 165), ylim = c(-74, -58.5), expand = FALSE)+
  geom_tile(data = CESM_df, mapping = aes(x = lon, y = lat, fill = as.factor(poly_id)), 
            show.legend = T)+
  geom_point(data = dives, aes(x=lon,y=lat),
             shape = 16, colour = "darkgrey",fill="NA", size = .1, alpha = .2,
             show.legend = TRUE) +
  scale_fill_manual(values = pal_CESM_met)+
  labs(x = "Lon (°E)",
       y = "Lat (°N)",
       fill = "Polynya id")+
  ggtitle("CESM")+
  theme_bw()


pdf(paste0(path_output, "map_polynya_id_CESM.pdf"), height = 10, width = 30)
print(map_polynya_id_CESM)
dev.off()
