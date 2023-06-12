assign_polygon_to_points <- function(df_pts, df_polygons, split_polygons) {
  val = data.frame("val" = rep(1, nrow(df_pts)))
  sp_dives = SpatialPointsDataFrame(cbind(df_pts$lon, df_pts$lat), data = val)
  var = colnames(df_polygons)
  rm_var <- var[!var %in% c("lon", "lat")]
  
  i = which(var == split_polygons)
  list_pol = split(df_polygons, f = df_polygons[ ,i])
  for (v in rm_var) {
    list_pol <- lapply(list_pol, function(x) { x[v] <- NULL; x })
  }
  
  p_pol <- lapply(list_pol, Polygon)
  ps_pol <- lapply(seq_along(p_pol), function(i) Polygons(list(p_pol[[i]]), ID = names(p_pol)[i]))
  sps_pol <- SpatialPolygons(ps_pol)
  
  id = over(sp_dives, sps_pol)
  df_pts$id = names(p_pol)[as.numeric(id)]
  return(df_pts)
}