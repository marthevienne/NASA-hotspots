get_polynya_id <- function(z, time, lon_dive, lat_dive, date) {
  lon_dive = as.numeric(lon_dive)
  lat_dive = as.numeric(lat_dive)
  date = as.Date(date)
  year = year(date)
  month = month(date)
  i_loc = rev(which(lat <= lat_dive))[1]
  j_loc = rev(which(lon <= lon_dive))[1]
  matrix_loc = matrix(data = NA, nrow = length(lat), ncol = length(lon))
  matrix_loc[i_loc, j_loc] = 1
  
  matrix_pol = t(get_contours_pol(z, time, year, month))
  
  id = matrix_pol[which(!is.na(matrix_loc * matrix_pol))]
  
  if (identical(id, integer(0)))
    return(0)
  else
    return(id)
}

############################################################################

get_polynya_id_typical <- function(lon, lat, z, lon_dive, lat_dive) {
  lon = sapply(lon, function(x) ifelse(x > 180, x - 360, x))
  lon_dive = as.numeric(lon_dive)
  if (lon_dive > 180) {
    lon_dive = lon_dive - 360
  }
  print(lon_dive)
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

############################################################################

get_contours_pol <- function(polynyas, time, year, month) {
  time_reformat = format(time, "%Y-%m")
  date <- paste0("01/0", month, "/", year)
  date <- as.POSIXlt(date, format="%d/%m/%Y")
  date_reformat = format(date, "%Y-%m")
  index = which(time_reformat  == date_reformat)
  dim_matrix = dim(polynyas)
  contours = polynyas[1 : dim_matrix[1], 1 : dim_matrix[2], index]
  return(contours)
}

############################################################################

plot_contour_polynyas_type <- function(polynyas, time, 
                                       date, 
                                       latlim = c(min(lat), max(lat)), 
                                       lonlim = c(min(lon), max(lon))) {
  year = year(date)
  month = month(date)
  colors = hcl.colors(3)
  colMap <- wes_palette("Zissou1", 5, type = "discrete")
  z = get_contours_pol(polynyas, time, year, month)
  #if (is_empty(z)) {
  if (no_pol(z)) {
    colMap <- wes_palette("Zissou1", 1, type = "discrete")
    z = readRDS("no_pol_raster")
    image(lon, lat, z, col = colMap, xlim = lonlim, ylim = latlim)
    colMap <- wes_palette("Zissou1", 5, type = "discrete")
    legend(grconvertX(0.5, "device"), grconvertY(1, "device"),
           c("fast-ice or ocean", "open-ocean polynyas","coastal polynyas"), fill = colMap[c(1,3,5)], xpd = NA) #change legend position
  } else {
    image(lon, lat, z, col = colMap, xlim = lonlim, ylim = latlim)
    legend(grconvertX(0.5, "device"), grconvertY(1, "device"),
           c("fast-ice or ocean", "open-ocean polynyas","coastal polynyas"), fill = colMap[c(1,3,5)], xpd = NA) #change legend position
  }
  if (nchar(month) == 2) {
    title(paste0(year, "-", month))
  }
  else {
    title(paste0(year, "-0", month))
  }
}

############################################################################

no_pol <- function(z) {
  if (nlevels(as.factor(z)) < 2) {
    return(TRUE)
  }
  return(FALSE)
}

############################################################################

plot_contour_polynyas_id <- function(polynyas, time, id_poly,
                                     date,
                                     latlim = c(min(lat), max(lat)), 
                                     lonlim = c(min(lon), max(lon))) {
  year = year(date)
  month = month(date)
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  pal = sample(color, max(id_poly)) # display in legend only pol present
  z = t(get_contours_pol(polynyas, time, year, month))
  fig <- plot_ly(x = lon, y = lat, z = z, colors = pal, type = "heatmap")
  fig <- layout(fig, xaxis = list(range = lonlim), yaxis = list(range = latlim))
  fig
  #image(lon, lat, z, col = pal)
  #legend(grconvertX(0.5, "device"), grconvertY(1, "device"),
  #      polyID, fill = pal, xpd = NA)
}
