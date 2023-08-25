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
setwd("~/Desktop/WHOI/Data/") # <========= MODIFY WD
## ---------------------------
## Library
library(vegan)
library(dplyr)
library(ggplot2)
library(rainbow)
library(rgl)
## ---------------------------
## Paths
## ---------------------------
## Functions
## ---------------------------
## Palettes
source("palettes/palette_polynya.R")
source("palettes/palette_season.R")
source("palettes/palette_month.R")
## ---------------------------

## TODO: prof hunting seg > 150 => perc ?

## Behavioral data (Y)
Y <- read.csv("varbehav.csv", header = T) %>%
  ungroup() %>%
  dplyr::select(!c(X)) %>%
  na.omit()

id_dives <- Y %>%
  pull(id)
rownames(Y) <- paste0("Dive_", id_dives)

Y <- Y %>%
  dplyr::select(!id)

## Environmental data (X)
X <- read.csv("varenv.csv", header = T) %>%
  ungroup() %>%
  dplyr::select(!c(X)) %>%
  filter(id %in% id_dives)

id <- X %>%
  pull(id)
rownames(X) <- paste0("Dive_", id)

X <- X %>%
  dplyr::select(!id)

## Metadata
meta <- read.csv("meta.csv", header = T) %>%
  ungroup() %>%
  dplyr::select(!c(X)) %>%
  filter(id %in% id_dives)

#==================================================================
# Step 1: dissimilarity matrix of Y
#==================================================================
D_gower = vegdist(Y, method  = "gower", binary = FALSE, diag = FALSE, upper = FALSE, na.rm = FALSE)
saveRDS(D_gower, "D_gower")

#==================================================================
# Step 2: Principal coordinate analysis (PCO) on D
#==================================================================
rm(list=ls())

D_gower <- readRDS("D_gower")

pcoa <- cmdscale(D_gower, k = 3, eig = T, add = T)
saveRDS(pcoa, "pcoa")

## Get coordinates PCO
pos <- pcoa$points
colnames(pos) <- c("pcoa1", "pcoa2", "pcoa3")

pos <- pos %>%
  as_tibble(rownames = "obs") 

## Add metadata for coloring
pos$pol <- meta$pol
pos$season <- meta$season
pos$month <- meta$month

## Palette and colors for 3D plot
type = "month" # season | pol | month
if (type = "month") {
  pal <- pal_month
  col <- pal_month[month.name[pos$month]]
} else if (type = "season") {
  pal <- pal_season
  col <- pal_season[pos$season]
} else {
  col <- pal[pos$pol]
}

#------------------------------------------------------------------
# Plot PCO
#------------------------------------------------------------------

pos %>%
  ggplot(aes(x = pcoa1, y = pcoa2, col = factor(type))) +
  scale_color_manual(values = pal)
geom_point() +
  theme_bw()

plot3d(pos$pcoa1, pos$pcoa2, pos$pcoa3, col = col, size = 4)

#==================================================================
# Step 2/3: PCO and CAP 
#==================================================================
#Function capscale is based on Legendre & Anderson (1999): 
#the dissimilarity data are first ordinated using metric scaling, 
#and the ordination results are analysed as rda
D_gower <- readRDS("D_gower")
cap <- capscale(D_gower ~ X, metaMDS = TRUE) # metaMDS = TRUE => procede to PCO/MDS before CAP
saveRDS(cap, "~/Desktop/WHOI/Data/canonical_analysis/cap")

#------------------------------------------------------------------
# Plot CAP
#------------------------------------------------------------------

plot(cap, type = "n")
text(cap, dis = "cn")
points(cap, pch = 21, col = "red", bg = "yellow", cex = 1.2)
text(cap, "species", col = "blue", cex = 0.8)

## Limited output of 'summary'
head(summary(cap), tail = 2)

## End script
rm(list=ls())
