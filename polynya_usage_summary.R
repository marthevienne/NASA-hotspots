##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-24
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

## Working directory
setwd("~/Desktop/WHOI/Data/output_data/")

## Path fig
path_fig = ("~/Dropbox/data/outputs_Marthe_2023/dives max depth polynyas/")

## Library
library(ggplot2)
library(lubridate)
library(dplyr)

## Palette polynyas
source("~/Desktop/WHOI/Codes/Marthe_to_be_validated/palette_polynya.R")

## Import dives data
dives <- readRDS("dive_metrics_V3")
str(dives)

## Winter dives
dives_w <- subset(dives, month(DE_DATE) >= 3 & month(DE_DATE) <= 10)

## Import polynyas info
polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/Esther/ID_polynyas_Esther.csv", sep = ";")
## As dictionary
polynya_info_dict <- as.list(polynya_info[,2])
names(polynya_info_dict) = polynya_info$ID
print(polynya_info_dict[[1]])

## Compute summary on polynya usage by month and by polynya
pol_summary_usage <- dives %>% 
  group_by(pol, month) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()
pol_summary_usage$month = factor(pol_summary_usage$month, levels = month.name)

str(pol_summary_usage)
plot_ndives_by_pol <- ggplot(pol_summary_usage, aes(x = factor(pol), fill = factor(pol))) + 
  geom_bar(aes(y = n_dives), stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal, name = "Polynyas", labels = polynya_info$Name)+
  xlab("Polynya")+
  ylab("Number of dives")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        # panel.grid.major.x = element_blank() ,
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
        )


## Compute summary on polynya usage by polynya
pol_summary_usage_year <- dives %>% 
  group_by(pol) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()

print(summary(pol_summary_usage_year$n_dives))
Q1 <- summary(pol_summary_usage_year$n_dives)[2] # 1st quantile
polynya_info[pol_summary_usage_year$pol[pol_summary_usage_year$n_dives < Q1], ] # polynyas showing # dives < Q1

## Add Q1 hline on barplot
plot_ndives_by_pol <- plot_ndives_by_pol +
  geom_hline(aes(yintercept =  Q1, linetype = "Q1"), colour= 'black') +
  scale_linetype_manual(name = "", values = 2, 
                        guide = guide_legend(override.aes = list(color = "black")))

ggsave(paste0(path_fig, "barplot_dives_per_pol.png"), height = 30, width = 25, units = c("cm"), dpi = 300)



## Compute summary on polynya usage IN WINTER by month and by polynya
pol_summary_usage_w <- dives_w %>% 
group_by(pol, month) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()
pol_summary_usage_w$month = factor(pol_summary_usage_w$month, levels = month.name)

str(pol_summary_usage_w)
plot_ndives_by_pol_w <- ggplot(pol_summary_usage_w, aes(x = factor(pol), fill = factor(pol))) + 
  geom_bar(aes(y = n_dives), stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal, name = "Polynyas", labels = polynya_info$Name)+
  xlab("Polynya")+
  ylab("Number of dives")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        # panel.grid.major.x = element_blank() ,
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )

  
## Compute summary on polynya usage by polynya
pol_summary_usage_all_winter <- dives_w %>% 
  group_by(pol) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()

print(summary(pol_summary_usage_all_winter$n_dives))
Q1_w <- summary(pol_summary_usage_all_winter$n_dives)[2] # 1st quantile
polynya_info[pol_summary_usage_all_winter$pol[pol_summary_usage_all_winter$n_dives < Q1], ] # polynyas showing # dives < Q1

## Add Q1 hline on barplot
plot_ndives_by_pol_w <- plot_ndives_by_pol_w +
  geom_hline(aes(yintercept =  Q1_w, linetype = "Q1"), colour= 'black') +
  scale_linetype_manual(name = "", values = 2, 
                        guide = guide_legend(override.aes = list(color = "black")))

ggsave(paste0(path_fig, "WINTER_barplot_dives_per_pol.png"), height = 30, width = 25, units = c("cm"), dpi = 300)


## Distribution of maximum dive depth for winter dives
dives_w$yday = yday(dives_w$DE_DATE)
max_depth = max(dives_w$MAX_DEP)

str(dives)
for (pol_id in polynya_info$ID) {
  dives_w_pol = subset(dives_w, pol == pol_id)
  if (nrow(dives_w_pol) > 0) {
    title = sprintf("Maximum depth of dives in %s polynya", polynya_info_dict[[pol_id]])
    distri_max_depth <- ggplot(dives_w_pol) +
      scale_y_reverse(limits = c(max_depth, 0)) +
      labs(x = "Day of the year", y = "Maximum depth dive (m)")+
      ggtitle(title) +
      geom_point(aes(x = yday(DE_DATE), y = MAX_DEP, col = REF), size = .05) +
      #facet_wrap(~pol)+
      theme_bw()+
      theme(legend.position = "none")+
      scale_x_continuous(limits = c(60, 305), breaks = seq(0, 365, by = 30))+
      scale_colour_viridis_d(option = "D")
    
    pdf(paste0(path_fig, sprintf("winter_dives_max_depth_polynya_%s.pdf", pol_id)),
        height = 5,
        width = 7)
    print(distri_max_depth)
    print(hist(dives_w_pol$MAX_DEP, breaks = 100,
               xlab = "Maximum depth (in m)",
               main = title))
    dev.off()
  }
}

dives_w$month = factor(dives_w$month, levels = month.name)

dives_w$depth_max_seg = NA
#dives_w$depth_max_seg
for (row in 1:nrow(dives_w)) {
  print(row)
  dive_T = dives_w[row, c('T0', 'T1', 'T2', 'T3', 'T4', 'Tf')]
  dive_D = dives_w[row, c('D0', 'D1', 'D2', 'D3', 'D4', 'Df')]
  T_diff = diff(as.matrix(dive_T)[1,])
  id = which(T_diff == max(T_diff))
  dives_w$depth_max_seg[row] = (dive_D[1, id + 1] + dive_D[1, id]) / 2
}

source("~/Desktop/WHOI/Codes/palettes/palette_month.R")

for (pol_id in polynya_info$ID) {
  dives_w_pol = subset(dives_w, pol == pol_id)
  title2 = sprintf("Maximum depth of dives in %s polynya", polynya_info_dict[[pol_id]])
  ndives = pol_summary_usage_all_winter$n_dives[pol_summary_usage_all_winter$pol == pol_id]
  # ndives_m = pol_summary_usage_w$n_dives[pol_summary_usage_w$pol == pol_id]
  ndives_m = pol_summary_usage_w$n_dives[pol_summary_usage_w$pol == pol_id]
  
  subt = sprintf("Number of winter dives: %s", ndives)
  d_months = intersect(month.name, unique(dives_w_pol$month))
  labs_month = paste0(d_months, " (n = ", ndives_m, ")") 
  if (nrow(dives_w_pol) > 0) {
    distri_max_depth <- ggplot(dives_w_pol) +
      scale_y_reverse(limits = c(max_depth, 0)) +
      labs(x = "Day of the year", y = "Maximum dive depth (m)", subtitle = subt)+
      ggtitle(title2) +
      geom_point(aes(x = yday, y = MAX_DEP, col = REF), size = .5) +
      #facet_wrap(~pol)+
      theme_bw()+
      theme(legend.position = "none")+
      scale_x_continuous(limits = c(60, 305), breaks = seq(0, 365, by = 30))+
      scale_colour_viridis_d(option = "D")
    
    title1 = sprintf("Density distribution of maximum depth of dives in %s polynya", polynya_info_dict[[pol_id]])
    dens_max_depth_byM <- ggplot(dives_w_pol) +
      geom_density(aes(y = -MAX_DEP, col = month), linewidth = .5) +
      theme_bw() +
      xlab("Maximum dive depth (m)") +
      ylab("Density")+
      labs(col = "Month") +
      scale_color_viridis_d(labels = labs_month)+
      #scale_fill_viridis_d(labels = labs_month)+
      ggtitle(title1)
      
    dens_max_depth_byM_bySES <- ggplot(dives_w_pol) +
      geom_density(aes(y = -MAX_DEP, col = REF), alpha = .6, linewidth = .5) +
      theme_bw() +
      xlab("Maximum dive depth (m)") +
      ylab("Density")+
      labs(col = "Month") +
      facet_grid(~month) +
      scale_color_viridis_d()
      ggtitle(title1)
    
    title2 = sprintf("Maximum depth of dives in %s polynya", polynya_info_dict[[pol_id]])
    pdf(paste0(path_fig, sprintf("winter_dives_max_depth_polynya_%s.pdf", pol_id)),
        height = 7,
        width = 7)
    print(distri_max_depth)
    print(dens_max_depth_byM)
    #print(dens_max_depth_byM_bySES)
    print(hist(dives_w_pol$MAX_DEP, breaks = 100,
               xlab = "Maximum depth (in m)",
               main = title2))
    dev.off()
  }
}


### Surface plot lat/lon/max depth
polynyas = read.csv("~/Desktop/WHOI/Data/polynyas_contours/Esther/Ester_polynyas_contours_bigger_df.csv")
pol_sub = subset(polynyas, ID == 12 & month %in% month.name[3:10])
pol_sub$month = factor(pol_sub$month, levels = month.name)

plot(pol_sub$lon, pol_sub$lat)

pol_sub$z = 0

dives_w_pol = subset(dives_w, pol == 12)
dives_w_pol$month = factor(dives_w_pol$month, levels = month.name)

library(plotly)

p <- plot_ly(data = dives_w_pol,  x = ~lon, y = ~lat, z = ~-MAX_DEP,
        marker = list(size = 1, color = "black")) %>% 
  add_markers() %>%
  add_paths(x = pol_sub$lon, y = pol_sub$lat, z = pol_sub$z, split = pol_sub$month)
p


ggplot(dives_w_pol) +
  geom_polygon(data = pol_sub, aes(x = lon, y = lat), fill = NA, col = "black") +
  geom_point(aes(x = lon, y = lat, col = -MAX_DEP), size = .1) +
  facet_grid(~month) +
  scale_color_viridis_c()
ggsave(paste0(path_fig, "WINTER_scatter_plot_Barrier_dives.png"), height = 20, width = 40, units = c("cm"), dpi = 300)



## Max depth distribution plots by month in a given polynya
title = sprintf("Density distribution of maximum depth of dives in %s polynya", polynya_info_dict[[pol_id]])
ggplot(dives_w_pol) +
  geom_density(aes(x = MAX_DEP, col = month), alpha = .6, linewidth = .5) +
  theme_bw() +
  xlab("Maximum depth dive (m)") +
  ylab("Density")+
  labs(col = "Month")+
  ggtitle(title)
print(pol_summary_usage_w[pol_summary_usage_w$pol == 12,])

ggplot(dives_w_pol) +
  geom_density(aes(x = lon, col = month), alpha = .6, linewidth = 1) +
  theme_bw()

ggplot(dives_w_pol) +
  geom_density(aes(x = lat, col = month), alpha = .6, linewidth = 1) +
  theme_bw()

# classic plot :
library(ggExtra)
p <- ggplot(dives_w_pol, aes(x = lon, y = lat, color = month)) +
  geom_polygon(data = pol_sub, aes(x = lon, y = lat, col = month), fill = NA) +
  geom_point(size = .2) +
  theme_bw()

# marginal density
p2 <- ggMarginal(p, type="density", groupColour = TRUE)
p2

## End script
rm(list=ls())

