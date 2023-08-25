raster_to_df <- function(r) {
  n_lyrs <- nlayers(r)
  r_df <- NULL
  
  #Progress bar
  n_iter = n_lyrs
  
  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")
  
  for (lyr in 1:n_lyrs) {
    
    df <- as.data.frame(r[[lyr]], xy = TRUE) %>% 
      na.omit()
    if (nrow(df) > 0) {
      colnames(df) = c("x", "y", "value")
      df$layer = names(r)[lyr]
      r_df <- rbind(r_df, df)
    }
    
    Sys.sleep(0.1) # Remove this line and add your code
    
    # Sets the progress bar to the current state
    setTxtProgressBar(pb, lyr)
  }
  
  close(pb) # Close the connection
  
  return(r_df)
}
