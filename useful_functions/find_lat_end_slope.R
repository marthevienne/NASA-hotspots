find_lat_end_slope <- function(transect) {
  cont_slope <- transect %>%
    filter(bathymetry <= -3500) %>%
    group_by(lon) %>%
    reframe("end_slope" = lat[1])
  
  if (length(cont_slope$end_slope) == 0) {
    print("The bathymetry never reaches the end slope threshold. Explore visually.")
    return(NA)
  }
  return(cont_slope$end_slope)
}
