get_grid_res <- function(lon, lat) {
  lon_r = (max(lon) - min(lon)) / length(lon)
  lat_r = (max(lat) - min(lat)) / length(lat)
  res = c("lon_r" = lon_r, "lat_r" = lat_r)
}

print_grid_res <- function(lonLat) {
  cat(sprintf("Lon: %.2f°E \nLat: %.2f°N", lonLat[1], lonLat[2]))
}
