update_seals_table <- function(dives, table, name_var) {
  summary <- dives %>%
    group_by(REF) %>%
    summarise(n_dives = n()) %>%
    as.data.frame()
  
  old_names <- colnames(table)

  if (name_var %in% old_names) {
    j = which(old_names == name_var)
    table[,j] <- 0
    for (seal in unique(table$REF)) {
      n_dives = summary$n_dives[summary$REF == seal]
      if (length(n_dives) == 1) {
        table[,j][table$REF == seal] <- n_dives
      }
    }
  } else {
    table$var <- 0
    for (seal in unique(table$REF)) {
      n_dives = summary$n_dives[summary$REF == seal]
      if (length(n_dives) == 1) {
        table$var[table$REF == seal] <- n_dives
      }
    }
    new_names <- c(old_names, name_var)
    colnames(table) <- new_names
  }
  
  return(table)
}
