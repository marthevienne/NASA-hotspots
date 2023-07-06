string_to_array <- function(string, numeric = FALSE) {
  array <- NULL
  for (char in 1:nchar(string)) {
    if (numeric) {
      res <-  substr(string, char, char) %>%
        as.numeric()
    } else {
      res <-  substr(string, char, char)
    }
    array <- c(array, res)
  }
  return(array)
}

## Tests
# str <- "1375318496"
# string_to_array(str, numeric = T)

# str <- "ZBREGE3532"
# string_to_array(str, numeric = F)

