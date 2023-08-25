## ACP
rm(list=ls())

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
source("plot_pca.R")
source("plot_percent_var.R")
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
  #filter(season == "Winter") %>%
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
library(FactoMineR)
data = as.data.frame(cbind(Y_ord, X_ord))

data$pol <- meta$pol
data <- data %>%
  arrange_at("pol")

col <- data$pol
pca <- prcomp(data, center=TRUE, scale=TRUE)
group = col

# data <- data %>%
#   dplyr::select(!pol)

i = which(colnames(data) == "pol")
data1.pca = PCA(data[,-i], scale=F, ncp = 2, graph = F)
eigenvalues <- data1.pca$eig

pdf("ACP/pca_behav_var_dim.pdf", height = 7, width = 7, pointsize = 15)
plot.PCA(data1.pca, choix = "var") # col.ind = pal[as.character(col)]
dev.off()

library("factoextra")

pdf("ACP/pca_dim_all_sea.pdf", height = 10, width = 10, pointsize = 15)
plot.PCA(data1.pca, choix = "ind", col.ind = pal[as.character(col)], label = "none")
plot.PCA(data1.pca, choix = "ind", habillage = as.character(col), col.hab = pal, label = "none")
fviz_pca_ind(data1.pca, label="none") +
  geom_point(shape = 21, aes(fill = as.factor(data$pol)), col = "black") +
  scale_fill_manual(values = pal)
dev.off()

# fviz_pca_biplot(data1.pca, 
#                 habillage = as.factor(data$pol), addEllipses = TRUE,
#                 col.var = "red", alpha.var ="cos2",
#                 label = "var") +
#   scale_color_brewer(palette="Dark2")+
#   theme_minimal()



data1.pca$var$cos2
data1.pca$var$contrib





data = cbind(Y_ord)
data = as.data.frame(data)
data$pol = as.character(meta$pol)

pdf("ACP/pca_behav_var_dim.pdf", height = 10, width = 10, pointsize = 15)
data1.pca = PCA(data, scale=TRUE, ncp = 4)
dev.off()


#######
# sum of contribution for each variable
test=NULL
namesvars=colnames(data)[1:nvar]
nvar = ncol(data) - 2
for(k in 1:nvar){
  tmp=sum(data1.pca$var$contrib[k,1:3])
  test=c(test,tmp)
}
test=as.data.frame(t(test))
names(test) <-namesvars

test2=NULL
for(k in 1:nvar){
  tmp=sum(data1.pca$var$contrib[k,1:2])
  test2=c(test2,tmp)
}
test2=as.data.frame(t(test2))
names(test2) <- namesvars

test3=NULL
for(k in 1:nvar){
  tmp=sum(data1.pca$var$contrib[k,1:4])
  test3=c(test3,tmp)
}
test3=as.data.frame(t(test3))
names(test3) <- namesvars

tot=rbind(test3,test2,test)
tot$type[1] = "4 components"
tot$type[2] = "2 components"
tot$type[3] = "3 components"

tot2=t(tot)
tot2=as.data.frame(tot2)
for (i in 1:nvar){}
tot2$variables=as.character(names(tot))
tot2=as.data.frame(tot2)
tot2[(nvar + 1):(nvar  + nvar),] = NA
tot2[(nvar + nvar + 1): (nvar + nvar + nvar),] = NA
tot2[(nvar + 1):(nvar  + nvar), 1 ] = tot2[1:nvar,2]
tot2[(nvar + nvar + 1):(nvar + nvar + nvar),1]=tot2[1:nvar,3]
tot2=tot2[,-2]
tot2=tot2[,-2]
tot2$type=NA
tot2$type[1:nvar]="First 4"
tot2$type[(nvar + 1) : (nvar + nvar)]="First 2"
tot2$type[(nvar + nvar + 1):(nvar + nvar + nvar)]="First 3"
tot2$variables[(nvar + 1) : (nvar + nvar)]=tot2$variables[1:nvar]
tot2$variables[(nvar + nvar + 1):(nvar + nvar + nvar)]=tot2$variables[(nvar + 1) : (nvar + nvar)]
rownames(tot2) <- NULL
tot2$type=as.factor(tot2$type)
tot2$V1=round(as.numeric(tot2$V1))

library(tidyverse)
tot3 <- tot2 %>%
  mutate(variables = factor(variables, levels = tot2 %>%
                              filter(type == "First 2") %>%
                              arrange(V1) %>%
                              dplyr::select(variables) %>%
                              pull()))
tot3b=tot3[tot3$type=="First 2",]

# figure 2b---------------------------------------------------------------------
ggplot(tot3b,
       aes(x = variables,
           y = V1,
           col = type)) +
  geom_text(aes(label = V1),
            nudge_x = 0.4) +
  geom_point()+
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "", x = "", y = "Variance contribution (%)", col = "") + #
  theme_minimal() +
  coord_flip()


#########
library(mclust)
pc1= as.data.frame(data1.pca$ind$coord)

head(pc1)
pc1$pol = meta$pol
pc1 <- pc1 %>%
  arrange_at("pol")

p <- ggplot() +
  scale_fill_manual("Polynya", values = pal) +
  theme_bw() +
  geom_point(data = pc1, aes(x = Dim.1, y = Dim.2, fill = factor(pol)), shape = 21, stroke=0.5, color = "black")
  

for (i in 1:length(coord)) {
  p <- p + geom_point(data = coord[[i]], aes(x = Dim.1, y = Dim.2, fill = factor(pol)), shape = 21, stroke=0.5, color = "black")
}

p

plot(pc1$Dim.1, pc1$Dim.2, col)

pc1.mbc=Mclust(pc1,G=5,modelNames = "VEI")

plot(pc1.mbc,what="classification")


########
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

col = c("0" = "orange",
        "1" = "#481B6DFF", 
        "2"= "#80CAAC" , 
        "3"= "#EE7496",
        "4" = "#348FA7FF")

meta$region = zone[as.character(meta$pol)]

source("plot_pca.R")
source("plot_percent_var.R")

## Polynya info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- polynya_info[,2] #___as dictionnary (fill labels)
names(polynya_info_dict) = polynya_info$ID

## All var
data = as.data.frame(cbind(Y_ord, X_ord))

data$pol <- meta$pol
data <- data %>%
  arrange_at("pol")

col <- data$pol
pca <- prcomp(data, center=TRUE, scale=TRUE)
group = col

pdf("ACP/pca_all_var_all_seasons.pdf", height = 10, width = 10, pointsize = 15)
plot_pca(pca, pc = 2, 
              conditions = group, 
              colours = pal, 
              size = 1.5,
              labels = polynya_info_dict, 
              title = "PCA (all variables and all seasons)")
plot_percent_var(pca, pc = 4)
dev.off()

## Behav data
data = as.data.frame(Y_ord)

data$pol <- meta$pol
data <- data %>%
  arrange_at("pol")

col <- data$pol
pca <- prcomp(data, center=TRUE, scale=TRUE)
group = col

pdf("ACP/pca_behav_var_all_seasons.pdf", height = 10, width = 10, pointsize = 15)
plot_pca(pca, pc = 2, 
         conditions = group, 
         size = 1.5,
         colours = pal, 
         labels = polynya_info_dict, 
         title = "PCA (behav variables and all seasons)")
plot_percent_var(pca, pc = 4)
dev.off()

## Env data
data = as.data.frame(X_ord)

data$pol <- meta$pol
data <- data %>%
  arrange_at("pol")

col <- data$pol
pca <- prcomp(data, center=TRUE, scale=TRUE)
group = col
pdf("ACP/pca_env_var_all_seasons.pdf", height = 10, width = 10, pointsize = 15)
plot_pca(pca, pc = 2, 
         conditions = group, 
         colours = pal, 
         size = 1.5,
         labels = polynya_info_dict, 
         title = "PCA (env variables and all seasons)")
plot_percent_var(pca, pc = 4)
dev.off()


## Plot3D
# pal = c("shelf" = "green",
#         "slope" = "blue")
scores <- as.data.frame(pca$x)
plot3d(scores$PC1, scores$PC2, scores$PC3, col = pal[as.character(meta$pol)], size = .2)
