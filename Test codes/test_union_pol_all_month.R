id = 12

polynyas_OBS_multi = read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/Matlab/OBS_multiple_polynyas_contours_df.csv")

polynyas_OBS_multi_m = subset(polynyas_OBS_multi, ID == id)
polynyas_OBS_multi_m$new_ID = paste0(polynyas_OBS_multi_m$month,".", polynyas_OBS_multi_m$ID, ".", polynyas_OBS_multi_m$num)
keep_num = polynyas_OBS_multi_m %>% count(new_ID) %>%
  filter(n > 3)

keep_pol = subset(polynyas_OBS_multi_m, new_ID %in% keep_num$new_ID)
toto <-  sfheaders::sf_polygon(
  obj = keep_pol
  , x = "lon"
  , y = "lat"
  , polygon_id = "new_ID"
) 

union <- st_union(toto) %>% st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

if (length(union[[1]][[1]]) < 4) {
  cont = union[[1]][[1]][[1]]
} else {
  cont = union[[1]][[1]]
}

pol_sub = as.data.frame(matrix(data = NA, nrow = nrow(cont), ncol = 3))
colnames(pol_sub) = c("lon", "lat", "z")
pol_sub$lon = cont[,1]
pol_sub$lat = cont[,2]

plot(pol_sub$lon, pol_sub$lat, type = "l")

pol_sub$z = 0

setwd("~/Desktop/WHOI/Data/output_data/")

## Import dive data
dives <- readRDS("dive_metrics_V3")
str(dives)

## Winter dives
dives_w <- subset(dives, month(DE_DATE) >= 3 & month(DE_DATE) <= 10)

margin = 0.5
bbox = c(min(pol_sub$lon) - margin, max(pol_sub$lon) + margin, min(pol_sub$lat) - margin, max(pol_sub$lat) + margin)

dives_w_ar_pol = subset(dives_w, lon > bbox[1] & lon < bbox[2] & lat > bbox[3] & lat < bbox[4])
dives_w_pol = subset(dives_w, pol == id)
dives_w_pol$month = factor(dives_w_pol$month, levels = month.name)

library(plotly)
library(viridis)
p <- plot_ly(data = dives_w_pol,  x = ~lon, y = ~lat, z = ~-MAX_DEP,
             marker = list(size = 1, color = "grey", colors ='Set1')) %>% 
  add_markers() %>%
  add_paths(x = pol_sub$lon, y = pol_sub$lat, z = pol_sub$z, split = pol_sub$month, line = list(color = "red"))
p

p <- plot_ly(
  data = dives_w_ar_pol,
  x = ~lon,
  y = ~lat,
  z = ~-MAX_DEP,
  type = "scatter3d",
  mode = "markers",
  color = ~month,
  colors = "viridis",
  marker = list(size = 1)
) %>%
 
p

ggplot(dives_w_pol) +
  geom_polygon(data = pol_sub, aes(x = lon, y = lat), fill = NA, col = "black") +
  geom_point(aes(x = lon, y = lat, col = -MAX_DEP), size = .1) +
  #facet_grid(~month) +
  scale_color_viridis_c()
