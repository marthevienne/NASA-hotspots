## ACP

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
  dplyr::select(!c(X)) %>%
  filter(season == "Winter") %>%
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
library(FactoMineR)

data1.pca=PCA(Y_ord,scale=TRUE,ncp=4)
data1.pca$var$cos2
data1.pca$var$contrib


#######
# sum of contribution for each variable
# test=NULL
# namesvars=colnames(Y_ord)
# for(k in 1:ncol(Y_ord)){
#   tmp=sum(data1.pca$var$contrib[k,1:3])
#   test=c(test,tmp)
# }
# test=as.data.frame(t(test))
# names(test) <-namesvars
# 
# test2=NULL
# namesvars=colnames(Y_ord)
# for(k in 1:ncol(Y_ord)){
#   tmp=sum(data1.pca$var$contrib[k,1:2])
#   test2=c(test2,tmp)
# }
# test2=as.data.frame(t(test2))
# names(test2) <- namesvars
# 
# test3=NULL
# namesvars=colnames(Y_ord)
# for(k in 1:ncol(Y_ord)){
#   tmp=sum(data1.pca$var$contrib[k,1:4])
#   test3=c(test3,tmp)
# }
# test3=as.data.frame(t(test3))
# names(test3) <- namesvars
# 
# tot=rbind(test3,test2,test)
# tot$type[1] = "4 components"
# tot$type[2] = "2 components"
# tot$type[3] = "3 components"
# 
# tot2=t(tot)
# tot2=as.data.frame(tot2)
# for (i in 1:ncol(Y_ord)){}
# tot2$variables=as.character(names(tot))
# tot2=as.data.frame(tot2)
# tot2[14:26,]=NA
# tot2[27:39,]=NA
# tot2[14:26,1]=tot2[1:13,2]
# tot2[27:39,1]=tot2[1:13,3]
# tot2=tot2[,-2]
# tot2=tot2[,-2]
# tot2$type=NA
# tot2$type[1:13]="First 4"
# tot2$type[14:26]="First 2"
# tot2$type[27:39]="First 3"
# tot2$variables[14:26]=tot2$variables[1:13]
# tot2$variables[27:39]=tot2$variables[1:13]
# rownames(tot2) <- NULL
# tot2$type=as.factor(tot2$type)
# tot2$V1=round(as.numeric(tot2$V1))
# 
# library(tidyverse)
# tot3 <- tot2 %>%
#   mutate(variables = factor(variables, levels = tot2 %>%
#                               filter(type == "First 2") %>%
#                               arrange(V1) %>%
#                               select(variables) %>%
#                               pull()))
# tot3b=tot3[tot3$type=="First 2",]
# 
# # figure 2b---------------------------------------------------------------------
# ggplot(tot3b, 
#        aes(x = variables, 
#            y = V1,
#            col = type)) + 
#   geom_text(aes(label = V1),
#             nudge_x = 0.4) +
#   geom_point()+
#   scale_colour_brewer(palette = "Dark2") + 
#   labs(title = "", x = "", y = "Variance contribution (%)", col = "") + #
#   theme_minimal() +
#   coord_flip()


#########
library(mclust)
pc1=data1.pca$ind$coord
pc1.mbc=Mclust(pc1,G=5,modelNames = "VEI")

plot(pc1.mbc,what="classification")

