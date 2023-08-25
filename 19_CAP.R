## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-08-17
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
setwd("~/Desktop/WHOI/Data/")
## ---------------------------
## Library
library(ade4)
library(vegan)
library(dplyr)
library("FactoMineR")
library(ggplot2)
library(CCA)
library(rainbow)
library(rgl)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------

## Behavioral data (Y)
Y <- readRDS("canonical_analysis/matrix_Y_CAP") %>%
  ungroup() %>%
  dplyr::select(!c(min_depth, max_depth)) #%>%
  filter(pol != 21)

Y %>%
  group_by(pol) %>%
  count()

Y_sample <- Y %>%
  group_by(pol) %>%
  sample_n(100)

pol <- Y_sample %>%
  dplyr::select(c(num_fs, pol)) 

## Environmental data (X)
X <- readRDS("canonical_analysis/matrix_X_CAP") %>%
  filter(num_fs %in% Y_sample$num_fs) %>%
  ungroup() %>%
  left_join(pol, by = "num_fs") %>%
  na.omit()

########
pol <- Y_sample %>% 
  dplyr::select(c(num_fs, pol)) %>%
  filter(num_fs %in% X$num_fs)

season <- X %>% 
  dplyr::select(c(num_fs, season))

month <- X %>% 
  dplyr::select(c(num_fs, month))
  
Y_sample <- Y_sample %>%
  ungroup() %>%
  filter(num_fs %in% X$num_fs) %>%
  dplyr::select(!c(num_fs, pol))

X <- X %>%
  dplyr::select(!num_fs)

## Step 1: dissimilarity matrix of Y
#D = dist.binary(Y_sample, method = 10)
# D = vegdist(x, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
#             na.rm = FALSE) 
D = dist(Y_sample, method = "euclidean", diag = FALSE, upper = FALSE, p = 0)
##
rownames(Y_sample) = paste0("FS_", seq(1, nrow(Y_sample)))
library(funrar)
D = compute_dist_matrix(
  Y_sample,
  metric = "gower",
  center = F,
  scale = FALSE
)

## Step 2: Principal coordinate analysis (PCO, metric MDS) on Y
pcoa <- cmdscale(D, k = 3, eig = T, add = T)
pos <- pcoa$points
colnames(pos) <- c("pcoa1", "pcoa2", "pcoa3")

pos <- pos %>%
  as_tibble(rownames = "obs") 

pos$pol <- pol$pol
pos$season <- season$season
pos$month <- month$month

source("~/Desktop/NASA-hotspots/palettes/palette_polynya.R")
source("~/Desktop/NASA-hotspots/palettes/palette_season.R")
source("~/Desktop/NASA-hotspots/palettes/palette_month.R")

col <- pal[pos$pol]
col <- pal_season[pos$season]
col <- pal_month[month.name[pos$month]]


pos %>%
  ggplot(aes(x = pcoa1, y = pcoa2, col = factor(pol))) +
  geom_point() +
  theme_bw()

plot3d(pos$pcoa1, pos$pcoa2, pos$pcoa3, col = col, size = 4)

## Step 3:
adonis2(D~MLD+water_mass, X)

# - dive scale
# - papier CAP
# - longest FS
# - min max mean T + S 
# - CESM: zoo max HS + phyto à dive
# - table + code + papier

## End script
rm(list=ls())
