
months = 3:8

m = 7
  ## Metadata
  meta <- read.csv("meta.csv", header = T) %>%
    ungroup() %>%
    dplyr::select(!c(X)) %>%
    filter(month == m) %>%
    #filter(pol != 0 & pol != 21) %>%
    mutate(id = paste0("Dive_", id))
  
  id_dives <- meta %>% pull(id)
  
  ## Behavioral data (Y)
  Y <- read.csv("varbehav.csv", header = T) %>%
    ungroup() %>%
    dplyr::select(!c(X)) %>%
    mutate(id = paste0("Dive_", id)) %>%
    filter(id %in% meta$id)
  
  rownames(Y) <- Y$id
  
  Y <- Y %>%
    dplyr::select(!id)
  
  ## Environmental data (X)
  X <- read.csv("varenv.csv", header = T) %>%
    ungroup() %>%
    dplyr::select(!c(X)) %>%
    mutate(id = paste0("Dive_", id)) %>%
    filter(id %in% meta$id)
  
  rownames(X) <- X$id
  
  X <- X %>%
    dplyr::select(!id)
  
  ## Ordering dataframes
  Y_ord <- Y[match(meta$id, rownames(Y)), ]
  
  X_ord <- X[match(meta$id, rownames(X)), ]
  
  X_ord <- X_ord %>%
    dplyr::select(!c(lon, lat, water_mass)) %>%
    scale()
  
  Y_ord <- Y_ord %>%
    dplyr::select(!c(SPEED_ASC, SPEED_DESC)) %>%
    scale()
  
  ########
  data = as.data.frame(cbind(Y_ord, X_ord))
  
  data$pol <- meta$pol
  data <- data %>%
    arrange_at("pol", desc)
  
  i = which(colnames(data) == "pol")
  
  data$pol <- as.character(data$pol)
  data$pol <- polynya_info_dict[as.character(data$pol)]

  
  res = MFA(data, group=c(5, 7, 1), 
            type=c("c", "c", "n"), 
            ncp=4, 
            name.group=c("behaviour","environment", "polynya"), 
            num.group.sup= 3, graph = F)
  
  plot(res,choix="ind", habillage = "pol", lab.ind = F)
  
  
  fviz_mfa_ind(res, geom = "point",
               habillage = "pol", # color by groups 
               palette = pal,
               addEllipses = TRUE, ellipse.type = "confidence", 
               repel = TRUE # Avoid text overlapping
  ) 
  
  names(pal) = c("pol.NA", as.array(polynya_info_dict))


