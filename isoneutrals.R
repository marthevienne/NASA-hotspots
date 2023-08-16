## Compute isoneutrals (isolines of same neutral density)
isoneutrals <- function(ison) {
  require(dplyr)
  matrix <- read.csv("~/Desktop/WHOI/Data/ctd_data/gamma_n_data/gamma_matrix.csv", sep = ";", header = F)
  colnames(matrix) = c("t", "SP", "gamma")
  
  res <- matrix %>%
    filter(gamma > ison - 1e-4 & gamma < ison + 1e-4)
  
  return(res)
}

## Test
# iso <- isoneutrals(28)
# isoBis <- isoneutrals(28.27)
# 
# ggplot() +
#   geom_line(data = iso, aes(x = SP, y = t)) +
#   geom_line(data = isoBis, aes(x = SP, y = t))

