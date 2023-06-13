df_to_SpatialPolygonsDataFrame <- function(df, split_polygons) {
  var = colnames(df)
  rm_var <- var[!var %in% c("lon", "lat")]
  
  i = which(var == split_polygons)
  list_pol = split(df, f = df[ ,i])
  for (v in rm_var) {
    list_pol <- lapply(list_pol, function(x) { x[v] <- NULL; x })
  }
  
  p_pol <- lapply(list_pol, Polygon)
  ps_pol <- lapply(seq_along(p_pol), function(k) Polygons(list(p_pol[[k]]), ID = names(p_pol)[k]))
  sps_pol <- SpatialPolygons(ps_pol)
  # print(plot(sps_pol))
  sps_df <- SpatialPolygonsDataFrame(sps_pol, data = data.frame(id = names(p_pol)), match.ID = F)
  return(sps_df)
}
