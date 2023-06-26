library(reshape2)

transform_to_raster_1lyr <- function(file_name, lon_name, lat_name, var_name, integral = F, ratio = F) {
  
  ncin <- ncdf4::nc_open(file_name)
  
  count2 <- c(-1, -1)
  
  # if (depth) {
  #   
  #   count1 <- c(-1, 70, depth_count, -1)
  #   
  #   z <- ncdf4::ncvar_get(ncin, var_name, count = count1, verbose = FALSE)
  #   
  #   if (integral) {
  #     
  #     z <- z*10
  #     z <- apply(z, c(1,2,4), sum)
  #     
  #   }
  #   
  # } else 
  z <- ncdf4::ncvar_get(ncin, var_name, count = count2, verbose = FALSE)
  
  lon <- ncdf4::ncvar_get(ncin, lon_name, verbose = FALSE)[,1]
  # This conversion of lon ensures MAPPPED sites and rasters overlap
  #lon <- sapply(lon, function(x) ifelse(x > 180, (x - 360), x))
  lat <- ncdf4::ncvar_get(ncin, lat_name, verbose = FALSE)[1,]
  
  if (ratio == T) {
    z[which(z<0)] <- NA
    z <- z/100
  }
  
  
  rownames(z) = lon
  colnames(z) = lat
  
  toto = melt(z)
  toto$value <- as.factor(toto$value)
  levels(toto$value)
  toto$value <- NULL
  colnames(toto) = c("lon", "lat")
  
  grid_coord <- toto
  idx <- which(!is.na(grid_coord[,1]) &
                 !is.na(grid_coord[,2]))
  grid_coord <- grid_coord[idx,]
  

  pixels <- SpatialPixelsDataFrame(points = grid_coord,
                                     data = data.frame(z = c(z)[idx]),
                                     tolerance = 5.36362e-05)
    
  r <- raster(pixels[,'z'])

  return(r)
}
