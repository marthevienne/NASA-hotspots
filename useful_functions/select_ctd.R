select_ctd <- function(x, data, radius) {
  dive_lon <- as.double(x[grep( "interpLon", names(x) )])
  dive_lat <- as.double(x[grep( "interpLat", names(x) )])
  dive_time <- x[grep( "DE_DATE", names(x) )][[1]]
  dive_num <- x[grep( "NUM", names(x) )][[1]]
  tab <- points_in_circle(
    data, 
    lon_center = dive_lon,
    lat_center = dive_lat,
    lon = interpLon,
    lat = interpLat,
    radius = radius
  ) %>%
    mutate(dist_t = abs(difftime(time, dive_time, "hours")),
           NUM = dive_num) %>%
    select(c(REF, NUM, station, distance_m, dist_t)) %>%
    arrange(dist_t) %>%
    filter(row_number() == 1)
  return(tab)
}