zonal_transect_bathy <- function(bathy, lon) {
  points_lon <- rep(lon, each = bathy@nrows)
  by = (ext@ymax - ext@ymin) / (bathy@nrows - 1)
  lat <- seq(ext@ymin, ext@ymax, by = abs(by))
  points_lat <- rep(lat, length(lon))
  
  transect <- as.data.frame(cbind(points_lon, points_lat))
  depth <- extract(bathy, transect)
  transect$depth <- depth
  colnames(transect) = c("lon", "lat", "bathymetry")
  
  return(transect)
}