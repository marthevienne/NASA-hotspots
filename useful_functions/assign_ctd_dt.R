assign_ctd_dt <- function(ctd_data, dive_data, dt_max) {
  all_dives <- dive_data %>%
    select(c(REF, NUM, DE_DATE, MAX_DEP))
  
  ctd_data <- ctd_data %>%
    select(c(REF, id_ctd, time, station, max_depth))
  
  res <- NULL
  for (i in 1:nrow(ctd_data)) {
    ctd <- ctd_data[i,]
    #print(paste0("Station ", ctd$station))
    ctd_time <- ctd$time
    ctd_id <- ctd$id_ctd
    
    full_table <- all_dives %>%
      full_join(ctd, by = "REF")
    
    full_table <- full_table %>%
      mutate(dt = abs(difftime(DE_DATE, time, units = "mins"))) %>%
      filter(as.numeric(dt) <= dt_max * 60) %>%
      filter(max_depth > MAX_DEP)
    
    res <- rbind(res, full_table)
  }
  
  res <- res %>%
    
    group_by(REF, NUM) %>%
    arrange(dt, .by_group = T) %>%
    filter(row_number() == 1)
  
  res <- all_dives %>%
    select(c(REF, NUM)) %>%
    left_join(res, by = c("REF", "NUM")) %>%
    select(c(REF, NUM, id_ctd, dt))
  
  return(res)
}

