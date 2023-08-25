## ACP
rm(list=ls())

## Working directory
setwd("~/Desktop/NASA-hotspots/NASA_CAP_Sara/") # <========= MODIFY WD
## ---------------------------
## Library
# library(vegan)
library(dplyr)
library(ggplot2)
# library(rainbow)
# library(rgl)
library(lubridate)
library(FactoMineR)
## ---------------------------
## Paths
## ---------------------------
## Functions
# source("plot_pca.R")
# source("plot_percent_var.R")
## ---------------------------
## Palettes
source("palettes/palette_polynya.R")
source("palettes/palette_season.R")
source("palettes/palette_month.R")
## ---------------------------

#------------------------------------------------------------------
# ACP per month (with Mertz and outside)
#------------------------------------------------------------------

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- polynya_info[,2] #___as dictionnary (fill labels)
names(polynya_info_dict) = polynya_info$ID

pal["0"] = "#0C0927FF"

## Behavioral variables ---------------
months = 3:8

for (m in months) {
  ## Metadata
  meta <- read.csv("meta.csv", header = T) %>%
    ungroup() %>%
    dplyr::select(!c(X)) %>%
    filter(month == m) %>%
    filter(year == 2012) %>%
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
  data = as.data.frame(cbind(Y_ord))
  
  data$pol <- meta$pol
  data <- data %>%
    arrange_at("pol", desc)
  
  i = which(colnames(data) == "pol")
  res.pca = PCA(data[,-i], scale=F, ncp = 3, graph = F)
  
  
  pdf(sprintf("ACP/by_month/behav/2012/pca_behav_var_inout_%s_2012.pdf", month.name[m]), height = 10, width = 10, pointsize = 15)
  print(
    fviz_pca_ind(res.pca, label="none") +
      geom_point(shape = 21, aes(fill = as.factor(data$pol)), col = "black", size = 2) +
      scale_fill_manual("Polynya", values = pal, labels = polynya_info_dict) +
      guides(fill = guide_legend(override.aes = list(size = 5)))
  )
  print(fviz_screeplot(res.pca, ncp=6))
  print(plot.PCA(res.pca, choix = "var")) # col.ind = pal[as.character(col)]
  dev.off()
  
  # ind <- get_pca_ind(res.pca)
  # coord <- ind$coord
  # 
  # open3d()
  # plot3d(coord[,1], coord[,2], coord[,3], col = pal[as.character(data$pol)])
  # bgplot3d({
  #   plot.new()
  #   title(main = month.name[m], line = 3)
  # })
}


## All variables ---------------
months = 3:8


for (m in months) {
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
  res.pca = PCA(data[,-i], scale=F, ncp = 3, graph = F)
  
  
  pdf(sprintf("ACP/by_month/behav/pca_all_var_inout_%s.pdf", month.name[m]), height = 10, width = 10, pointsize = 15)
  print(
    fviz_pca_ind(res.pca, label="none") +
      geom_point(shape = 21, aes(fill = as.factor(data$pol)), col = "black", size = 2) +
      scale_fill_manual("Polynya", values = pal, labels = polynya_info_dict) +
      guides(fill = guide_legend(override.aes = list(size = 5)))
  )
  print(fviz_screeplot(res.pca, ncp=6))
  print(plot.PCA(res.pca, choix = "var")) # col.ind = pal[as.character(col)]
  dev.off()
  
  # ind <- get_pca_ind(res.pca)
  # coord <- ind$coord
  # 
  # open3d()
  # plot3d(coord[,1], coord[,2], coord[,3], col = pal[as.character(data$pol)])
  # bgplot3d({
  #   plot.new()
  #   title(main = month.name[m], line = 3)
  # })
}


## Environmental variables ---------------
months = 3:8

for (m in months) {
  ## Metadata
  meta <- read.csv("meta.csv", header = T) %>%
    ungroup() %>%
    dplyr::select(!c(X)) %>%
    filter(month == m) %>%
    filter(pol != 0 & pol != 21) %>%
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
  data = as.data.frame(cbind(X_ord))
  
  data$pol <- meta$pol
  data <- data %>%
    arrange_at("pol")
  
  i = which(colnames(data) == "pol")
  res.pca = PCA(data[,-i], scale=F, ncp = 3, graph = F)
  
  
  pdf(sprintf("ACP/by_month/env/pca_%s_env_var.pdf", month.name[m]), height = 10, width = 10, pointsize = 15)
  print(
    fviz_pca_ind(res.pca, label="none") +
      geom_point(shape = 21, aes(fill = as.factor(data$pol)), col = "black", size = 2) +
      scale_fill_manual("Polynya", values = pal, labels = polynya_info_dict) +
      guides(fill = guide_legend(override.aes = list(size = 5)))
  )
  print(fviz_screeplot(res.pca, ncp=6))
  print(plot.PCA(res.pca, choix = "var")) # col.ind = pal[as.character(col)]
  dev.off()
  
  ind <- get_pca_ind(res.pca)
  coord <- ind$coord
  
  open3d()
  plot3d(coord[,1], coord[,2], coord[,3], col = pal[as.character(data$pol)])
  bgplot3d({
    plot.new()
    title(main = month.name[m], line = 3)
  })
}

