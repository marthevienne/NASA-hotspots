identify_grouped_cells_matrix <- function(z) {
  source("~/Desktop/WHOI/Codes/functions_polynyas/master_to_raster.R")
  ## Check if group of cells cut in half in raster
  if (length(which(is.na(z)[,1] == T)) != 0) {
    print("Group might be cut in half... Rearrange matrix if continuous longitude scale") # TODO test start->end
  }
  
  r <- matrix_to_raster(z)
  
  id_gp = 0
  while (length(which(is.na(values(r) > 0)))) {
    id_gp = id_gp + 1
    
    same_gp_cells = which(is.na(values(r)))[1]
    r[same_gp_cells] = id_gp
    
    while (length(same_gp_cells) > 0) {
      adj_cells = adjacent(r, same_gp_cells, directions = 8, pairs = FALSE)
      same_gp_cells = adj_cells[which(is.na(values(r)[adj_cells]))]
      r[same_gp_cells] = id_gp
    }
  }
  
  r[which(values(r) == 0)] = NA
  
  m <- matrix(values(r), ncol = ncol(z), nrow = nrow(z))
  rotate1 <- m[, ncol(m):1]

  id_matrix = rotate1

  return(id_matrix)
}

