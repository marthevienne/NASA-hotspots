rearrange_spatial_matrix_360 <- function(lon, lat, z) {
  index_start_lon360 = which(lon == min(lon))
  seq_lon_360 = c(seq(index_start_lon360, nrow(z)), 
                  1:(index_start_lon360 - 1))
  lon_ctr180_360 = lon[seq_lon_360]
  z_rearranged_360 = z[seq_lon_360, ]
  
  res = list("lon" = lon_ctr180_360, "lat" = lat, "z" = z_rearranged_360)
  return(res)
}