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
setwd("~/Desktop/NASA-hotspots/NASA_CAP_Sara/") # <========= MODIFY WD
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

## Metadata
meta <- read.csv("meta.csv", header = T) %>%
  ungroup() %>%
  filter(season %in% c("Winter", "Autumn")) %>%
  dplyr::select(!c(X)) %>%
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
  dplyr::select(!c(water_mass)) %>%
  scale() %>%
  as_data_frame()

Y_ord <- Y_ord %>%
  scale() %>%
  as_data_frame()

rm(Y, X)

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- polynya_info[,2] #___as dictionnary (fill labels)
names(polynya_info_dict) = polynya_info$ID


#==================================================================
# Step 1: dissimilarity matrix of Y
#==================================================================
D_gower = vegdist(Y_ord, method  = "gower", binary = FALSE, diag = FALSE, upper = FALSE, na.rm = FALSE)
saveRDS(D_gower, "inside_outside/D_gower_all")

#==================================================================
# Step 2: Principal coordinate analysis (PCO) on D
#==================================================================
rm(list=ls())

D_gower <- readRDS("inside_outside/D_gower_all")

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
D_gower <- readRDS("inside_outside/D_gower_all")
cap <- capscale(D_gower ~ bathy + minS + maxS + meanS + minT + maxT + meanT, data = X_ord, metaMDS = F) # metaMDS = TRUE => procede to PCO/MDS before CAP
saveRDS(cap, "inside_outside/cap_all")

#------------------------------------------------------------------
# Plot CAP polynya
#------------------------------------------------------------------
pal["0"] = "blue"
col = pal[as.character(meta$pol)]
select_pal = pal[c("9", "10", "11", "12", "13", "14", "16", "17", "21")]
labels = c("0" = "outside", polynya_info_dict[names(select_pal)])
select_pal = pal[c("0", "9", "10", "11", "12", "13", "14", "16", "17", "21")]

scores = scores(cap)
coord = as.data.frame(scores$sites)
coord$pol = meta$pol
coord$col = pal[as.character(coord$pol)]

png("inside_outside/cap_all_seasons_pol_inout.png", height = 30, width = 35, units = "cm", res = 300)
print(plot(cap, type = "n"))
print(text(cap, dis = "cn"))
print(points(coord, pch = 3, col = col , bg = "yellow", cex = .5))
print(legend(x = -10, y = 0, col = select_pal, legend = labels, pch = 16))
print(text(cap, "species", col = "blue", cex = 0.8))
dev.off()


#------------------------------------------------------------------
# Plot CAP by region
#------------------------------------------------------------------
zone <- c("0" = "0",
          "9" = "1",
          "10" = "1",
          "11" = "1",
          "12" = "1",
          "13" = "1",
          "14" = "2",
          "16" = "3",
          "17" = "3",
          "21" = "4"
)


col = c("outside" = "orange",
        "Prydz Bay" = "#481B6DFF", 
        "Vincennes Bay\nand Cape Poinsett"= "#80CAAC" , 
        "Shackleton"= "#EE7496",
        "Mertz" = "#348FA7FF")

scores = scores(cap)
coord = as.data.frame(scores$sites)
coord$pol = meta$pol
coord$region = zone[as.character(coord$pol)]
pts0 = coord %>% filter(region == "0")
pts1 = coord %>% filter(region == "1")
pts2 = coord %>% filter(region == "2")
pts3 = coord %>% filter(region == "3")
pts4 = coord %>% filter(region == "4")

png("inside_outside/cap_winter_region_inout.png", height = 30, width = 35, units = "cm", res = 300)
print(plot(cap, type = "n"))
print(text(cap, dis = "cn"))
points(pts1, pch = 3, col = col["Prydz Bay"], bg = "yellow", cex = .5)
points(pts3, pch = 3, col = col["Vincennes Bay\nand Cape Poinsett"], bg = "yellow", cex = .5)
points(pts2, pch = 3, col = col["Shackleton"], bg = "yellow", cex = .5)
points(pts0, pch = 3, col = col["outside"], bg = "yellow", cex = .5)
points(pts4, pch = 3, col = col["Mertz"], bg = "yellow", cex = .5)
print(legend(x = -10, y = 0, col = col, legend = names(col), pch = 16))
print(text(cap, "species", col = "blue", cex = 0.8))
dev.off()



#------------------------------------------------------------------
# Canonical correlation
#------------------------------------------------------------------
library(CCA)
cc_results <- cancor(Y_ord, X_ord)

cc_results$xcoef
cc_results$ycoef
cc_results$cor

rownames(X_ord) = NULL
CC1_X <- as.matrix(X_ord) %*% cc_results$ycoef[, 1]
CC1_Y <- as.matrix(Y_ord) %*% cc_results$xcoef[, 1]

CC2_X <- as.matrix(X_ord) %*% cc_results$ycoef[, 2]
CC2_Y <- as.matrix(Y_ord) %*% cc_results$xcoef[, 2]

cor(CC1_X, CC1_Y)

assertthat::are_equal(cc_results$cor[1], 
                      cor(CC1_X,CC1_Y)[1])

cca_df <- meta %>% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)


cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y))+
  geom_point()

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- polynya_info[,2] #___as dictionnary (fill labels)
names(polynya_info_dict) = polynya_info$ID


cca_df$region = zone[as.character(meta$pol)]


cca_df %>% 
  ggplot(aes(x=CC2_X,y=CC2_Y, col=factor(pol))) +
  geom_point() +
  
  
  cca_df %>% 
  ggplot(aes(x=factor(pol),y=CC1_X, col=factor(pol)))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  theme(legend.position="none")

cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color = factor(pol, levels = names(polynya_info_dict))))+
  geom_point(alpha = .8, shape = 16) +
  scale_color_manual("Polynya", values = pal, labels = polynya_info_dict) +
  theme_bw()

## Palette and colors for 3D plot
type = "inside" # season | pol | month
if (type == "month") {
  pal <- pal_month
  col <- pal_month[month.name[cca_df$month]]
} else if (type == "season") {
  pal <- pal_season
  col <- pal_season[cca_df$season]
} else if (type == "pol") {
  col <- pal[cca_df$pol]
} else {
  pal <- c("inside" = "blue", "outside" = "green")
  col <- pal[cca_df$inside]
}

plot3d(cca_df$CC1_X, cca_df$CC2_X, cca_df$CC3_X, col = col, size = 4)

## Limited output of 'summary'
head(summary(cap), tail = 2)

## End script
rm(list=ls())
