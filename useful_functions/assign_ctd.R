assign_ctd <- function(ctd_data, dive_data, radius) {
  res <- NULL
  for (i in 1:nrow(ctd_data)) {
    ctd <- ctd_data[i,]
    ctd_lon <- ctd$interpLon
    ctd_lat <- ctd$interpLat
    ctd_time <- ctd$time
    ctd_station <- ctd$station
    ctd_depth <- ctd$max_depth
    tab <- points_in_circle(
      dive_data, 
      lon_center = ctd_lon,
      lat_center = ctd_lat,
      lon = interpLon,
      lat = interpLat,
      radius = radius
    ) %>%
      mutate(dist_t = abs(difftime(DE_DATE, ctd_time, "hours")),
             station = ctd_station,
             max_depth_ctd = ctd_depth) %>%
      select(c(REF, NUM, station, distance_m, dist_t, max_depth_ctd, MAX_DEP)) %>%
      arrange(dist_t)
    res <- rbind(res, tab)
  }
  return(res)
}

## Test
# library(dplyr)
# library(spatialrisk)
# ref <- c("ct109-939-14", "ct78-706-12", "ct140-635BAT-13")
# ctd <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table_north_bound_interp") %>%
#   # filter(row_number() <= 1000) %>%
#   filter(REF %in% ref)
# dives <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_V8") %>%
#   # filter(row_number() <= 1000) %>%
#   filter(REF %in% ref)
# 
# comp_table <- NULL
# for (id in ref) {
#   print(id)
#   ctd_seal <- ctd %>%
#     filter(REF == id)
#   
#   dives_seal <- dives %>%
#     filter(REF == id)
#   
#   tab_ctd <- assign_ctd(ctd_seal, dives_seal, 5000)
#   comp_table <- rbind(comp_table, tab_ctd)
# }
# 
# tab_ctd_filt <- comp_table %>%
#   group_by(REF, NUM) %>%
#   arrange(dist_t) %>%
#   filter(row_number() == 1) %>%
#   filter(dist_t <= 720) %>% #___min (12h)
#   filter(max_depth_ctd >= MAX_DEP)
# 
# nrow(tab_ctd_filt) / nrow(dives) * 100
# 
# full_tab <- tab_ctd_filt %>% left_join(dives, by = c("REF", "NUM")) %>%
#   select(c(REF, NUM, station, interpLon, interpLat))
# 
# library(ggplot2)
# ggplot() +
#   geom_point(data = full_tab, aes(x = interpLon, y = interpLat, col = factor(station)), shape = 16) +
#   # scale_color_viridis_d() +
#   theme(legend.position = "none")+
#   geom_point(data = ctd, aes(x = interpLon, y = interpLat), shape = 3)
# 
# test <- full_tab %>%
#   ungroup() %>%
#   group_by(station) %>%
#   reframe(max_lat = max(interpLat),
#           min_lat = min(interpLat),
#           max_lon = max(interpLon),
#           min_lon = min(interpLon),
#           dist = haversine(lat_from = max_lat, lon_from = max_lon, lat_to = min_lat, lon_to = min_lon))

