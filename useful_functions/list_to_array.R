source("~/Desktop/NASA-hotspots/useful_functions/string_to_array.R")
list_to_array <- function(list, numeric = FALSE) {
  array <- NULL
  for (el in list) {
    array <- c(array, string_to_array(el, numeric))
  }
  return(array)
}

## Test
list <- list("4253", "3240", "23539")
list_to_array(list, numeric = T)
