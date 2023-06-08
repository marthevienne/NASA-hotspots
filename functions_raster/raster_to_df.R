raster_to_df <- function(r) {
  n_lyrs <- nlayers(r)
  r_df <- NULL
  for (lyr in 1:n_lyrs) {
    df <- as.data.frame(r[[lyr]], xy = TRUE) %>% 
      na.omit()
    colnames(df) = c("x", "y", "value")
    df$layer = names(r)[lyr]
    r_df <- rbind(r_df, df)
  }
  return(r_df)
}