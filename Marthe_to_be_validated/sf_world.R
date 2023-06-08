library(tidyverse)
library(rnaturalearth)
library(sf)

world <- map_data('world') #Get polygon data

antarctica <- world %>% 
  filter(region == 'Antarctica')

data = data.frame(lat = -86, long = 0) #Create geographic data with southerly value

#plot
p_polygon <- ggplot() +
  geom_polygon(data = world, 
               aes(y = lat, x = long, group = group), col = 'grey', fill = NA) +
  geom_point(data = data, 
             aes(y = lat, x = long), col = 'red')

# use rnaturalearth package to get coastline data in the sf format
world_sf <- ne_coastline(returnclass = 'sf')
antarctica_sf <- ne_coastline(returnclass = 'sf')

# use geom_sf to an the data
p_sf <- ggplot(world) + 
  geom_sf(aes(x = long, y = lat))

library(cowplot)
# geom_sf, using an orthographic projection
p_sf_ortho <- ggplot(world_sf) +
  geom_sf()+  
  coord_sf( crs= "+proj=ortho +lat_0=-80 +lon_0=90")  

# the three plots together
cowplot::plot_grid(p_polygon, p_sf, p_sf_ortho,
                   labels = c('polygon', 'sf', 'sf orthographic'))


###############
install.packages("SOmap", repos = c(SCAR = "https://scar.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org"))

library(SOmap)
SOmap()

SES <- readRDS("interpoled_locations_dives_north_boundary_5meters")

ellie <- SOmap_data$mirounga_leonina

## construct and plot the map
SOmap_auto(SES$START_LON, SES$START_LAT)


###########
require(rnaturalearth)
world <- ne_countries(scale = "large", returnclass = "sf")
world_map <- world %>% 
  dplyr::group_by(continent) %>% 
  dplyr::select(continent)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data=world_map, col=2, alpha=.8)+
  coord_sf(xlim = c(-2, 170), ylim = c(-74, -58.5), expand = FALSE)+
  borders("world",alpha=.3, fill="grey",colour="black")+
  theme_bw()
