identify_grouped_cells_raster <- function(r) {
  ## Check if group of cells cut in half in raster
  m = matrix(data = values(r), nrow = r@nrows, ncol = r@ncols, byrow = T)
  
  if (length(which(!is.na(m[,1]))) != 0) {
    print("Group might be cut in half... Rearrange matrix if continuous longitude scale") # TODO test start->end
  }
  
  r[which(is.na(values(r)))] = 0
  r[which(values(r) != 0)] = NA
  
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
  
  return(r)
}
