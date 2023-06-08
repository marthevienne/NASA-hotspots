get_polynya_id_typical <- function(lon, lat, z, lon_dive, lat_dive) {
  lon = sapply(lon, function(x) ifelse(x > 180, x - 360, x))
  lon_dive = as.numeric(lon_dive)
  if (lon_dive > 180) {
    lon_dive = lon_dive - 360
  }
  lat_dive = as.numeric(lat_dive)
  i_loc = rev(which(lat <= lat_dive))[1]
  j_loc = rev(which(lon <= lon_dive))[1]
  matrix_loc = matrix(data = NA, nrow = length(lat), ncol = length(lon))
  matrix_loc[i_loc, j_loc] = 1
  
  matrix_pol = t(z)
  
  id = matrix_pol[which(!is.na(matrix_loc * matrix_pol))]
  
  if (identical(id, integer(0)) || identical(id, numeric(0))) {
    return(0)
  }
  return(id)
}
