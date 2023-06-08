# Test function
# library(reshape2)
# file = "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/ssmi_cdr_typical_polynya_mask_85%thresh"
# r = netcdf_to_raster(file_name = paste0(file, ".nc"),
#                         lon_name = "TLONG_sub",
#                         lat_name = "TLAT_sub",
#                         var_name = "polynya_typical_all",
#                         n_years = 1,
#                         n_months = 1,
#                         tolerance = 0.182704
# )
# plot(r)


netcdf_to_raster <- function(file_name, 
                                        lon_name, 
                                        lat_name, 
                                        var_name,
                                        n_years,
                                        n_months,
                                        tolerance) {
  
  ncin <- ncdf4::nc_open(file_name)
  
  z <- ncdf4::ncvar_get(ncin, var_name, verbose = FALSE)
  
  lon <- ncvar_get(ncin, varid = lon_name)
  # This conversion of lon ensures MAPPPED sites and rasters overlap
  lat <- ncvar_get(ncin, varid = lat_name)
  
  n_dim_coord = length(dim(lon))
  if (n_dim_coord != 1) {
    lon = lon[,1]
    lat = lat[1,]
  }
  
  lon <- sapply(lon, function(x) ifelse(x > 180, (x - 360), x))
  
  rownames(z) = lon
  colnames(z) = lat
  
  if (length(dim(z)) == 2) {
    z_df = melt(z)
  } else {
    z_df = melt(z[,,1])
  }
  z_df$value <- as.factor(z_df$value)
  levels(z_df$value)
  z_df$value <- NULL
  colnames(z_df) = c("lon", "lat")
  
  grid_coord <- z_df
  idx <- which(!is.na(grid_coord[,1]) &
                 !is.na(grid_coord[,2]))
  grid_coord <- grid_coord[idx,]
  
  if (n_years == 1 & n_months == 1) {
    
    pixels <- SpatialPixelsDataFrame(points = grid_coord,
                                     data = data.frame(z = c(z)[idx]),
                                     tolerance = tolerance)
    
    r <- raster(pixels[,'z'])
    
  } else {
    
    r <- list()
    a <- 1
    for (h in 1:(n_years*n_months)) {
      
      pixels <- SpatialPixelsDataFrame(points = grid_coord,
                                       data = data.frame(z = c(z[,,h])[idx]),
                                       tolerance = tolerance)
      
      r[[a]] <- raster(pixels[,'z'])
      a <- a + 1
      
    }
    r <- do.call(brick, r)
  }
  
  return(r)
}
