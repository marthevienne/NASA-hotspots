dist_km <- function(lat1, lat2, lon1, lon2) {
  require(okara)
  mean_lat <- mean(c(lat1, lat2))
  deg <- sqrt((abs(lon2) - abs(lon1))^2 + (abs(lat2) - abs(lat1))^2)
  res <- d2km(deg, base.latitude = mean_lat)
  return(res)
}

dist_km <- function(dlat, dlon) {
  require(okara)
  mean_lat <- mean(dlat)
  deg <- sqrt(abs(dlon)^2 + abs(dlat)^2)
  res <- d2km(deg, base.latitude = mean_lat)
  return(res)
}
