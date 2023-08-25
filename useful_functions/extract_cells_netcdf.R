extract_cells_netcdf <- function(file_nc, file_clip, 
                                 lon_name,
                                 lat_name,
                                 var_name,
                                 years, depths, months,
                                 tolerance) {

  require(stringr)
  require(terra)
  
  n_depths = length(depths)
  n_years = length(years)
  n_months = length(months)
  
  r <- netcdf_to_raster(
    file_nc,
    lon_name,
    lat_name,
    var_name,
    n_years,
    n_depths,
    n_months,
    tolerance
  )
  
  lyr_names <- paste0(rep(rep(month.name[months], each = n_depths), n_years), ".", 
                      rep(years, each = n_months * n_depths), ".", 
                      rep(depths, n_years * n_months))
  length(lyr_names)
  names(r) <- lyr_names
  
  # Extract cells
  cells_r <- raster::brick(file_clip)
  
  i = 1
  r_new <- NULL
  
  for (lyr in names(r)) {
    print(lyr)
    
    lyr_r <- which(names(r) == lyr)
    z <- r[[lyr_r]]
    
    lyr_month <- str_extract(lyr, "[A-z]*.[0-9]*")
    lyr_c <- which(names(cells_r) == lyr_month)
    p <- cells_r[[lyr_c]]
    p[!is.na(p)] = 1
    
    z <- crop(z, p)
    
    r_c_z <- z * p
    
    r_new[[i]] <- r_c_z
    
    i = i + 1
  }
  
  r_new <- do.call(brick, r_new)
  #r_new <- rast(r_new)
  names(r_new) <- lyr_names
  
  df <- raster_to_df(r_new)
  
  return(list(r_new, df))
}
