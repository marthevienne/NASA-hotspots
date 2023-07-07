update_seals_table <- function(df, seals, name_var) {
  summary <- df %>%
    group_by(REF) %>%
    summarise(n = n()) %>%
    as.data.frame()
  
  old_names <- colnames(seals)
  
  if (name_var %in% old_names) {
    j = which(old_names == name_var)
    seals[,j] <- 0
    for (seal in unique(seals$REF)) {
      n = summary$n[summary$REF == seal]
      if (length(n) == 1) {
        seals[,j][seals$REF == seal] <- n
      }
    }
  } else {
    seals$var <- 0
    for (seal in unique(seals$REF)) {
      n = summary$n[summary$REF == seal]
      if (length(n) == 1) {
        seals$var[seals$REF == seal] <- n
      }
    }
    new_names <- c(old_names, name_var)
    colnames(seals) <- new_names
  }
  
  return(seals)
}
