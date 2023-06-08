matrix_to_raster <- function(m) {
  rotate1 <- t(m)
  rotate2 <- rotate1[nrow(rotate1):1,]
  
  r = raster(rotate2)
  
  return(r)
}