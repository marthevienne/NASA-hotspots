##
## Script name: overlap_polynyas_CESM.R
##
## Purpose of script: Compute mean number of cells identified as polynyas by month that fall in polynya polygons
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-12
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes: To have an idea of how many cells fall in individual polynyas polygons, the identification of polynya cells is required.
##        Not done here.
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Working directory
## ---------------------------
## Library
library(raster)
library(ncdf4)
library(dplyr)
library(stringr)
library(terra)
library(tidyr)
library(ggplot2)
## ---------------------------

#==================================================================
# MONTHLY NCAR polynyas 
#==================================================================
source("~/Desktop/WHOI/Codes/functions_raster/netcdf_to_raster.R")

file <- "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.hi_0.4mthresh.polynya_sh.195801-202112"

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

years = seq(1958, 2021)

r <- netcdf_to_raster(
  file_name = paste0(file, ".nc"),
  lon_name = "tlon1d",
  lat_name = "tlat1d",
  var_name = "polynyas",
  n_years = length(years),
  n_months = 12,
  tolerance = 0.0001
)

lyr_names <- paste0(rep(month.name, length(years)), ".", rep(years, each = 12))
names(r) <- lyr_names
r

r[r == 0] = NA #___open ocean to NA
plot(r$January.1958) #___1 = open-ocean polynyas and 2 = coastal polynyas

start <- (2004 - years[1]) * 12 + 1
end <- (2019 - years[1] + 1) * 12
r_sub <- subset(r, start:end)

r_sub <- rast(r_sub)
terra::writeRaster(r_sub, "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_monthly_polynya_2004_2019.tiff", overwrite = T)

#==================================================================
# POLYNYAS DEFINED BY OBSERVATION 
#==================================================================
rm(list=ls())

source("~/Desktop/WHOI/Codes/useful_functions/df_to_SpatialPolygonsDataFrame.R")

r <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_monthly_polynya_2004_2019.tiff")

df_polynyas <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

df_polynyas <- df_polynyas %>%
  mutate(new_id = paste0(ID, "-", num),
         date = paste0(month, ".", year))

id_pols <- unique(df_polynyas$ID)
dates <- unique(names(r)) #___ordered array
dates <- dates[dates %in% unique(df_polynyas$date)] #___keep only dates present in df_polynyas

summary <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = length(dates),
                          ncol = 5))
colnames(summary) = c("layer", "n_cells_NCAR", "n_cells_overlap", "year", "month")

i = 1
r_new <- list()
bbox <- extent(c(min(df_polynyas$lon), max(df_polynyas$lon), -70, -60)) #___crop the polynya grid to the area where polynyas polygons are found

for (d in dates) {
  print(i)
  
  df_polynyas_lyr <- df_polynyas %>%
    filter(date == d)
  
  keep_poly <- df_polynyas_lyr %>%
    group_by(new_id) %>%
    summarise(n = n()) %>%
    filter(n > 4) %>%
    pull(new_id)
  
  df_polynyas_lyr <- df_polynyas_lyr %>% filter(new_id %in% keep_poly)
  
  SPDF <- df_to_SpatialPolygonsDataFrame(df_polynyas_lyr, split_polygons = "new_id")
  
  lyr_raster <- which(names(r) == d)
  r2 <- r[[lyr_raster]]
  r3 <- crop(r2, bbox)
  
  if (nrow(df_polynyas_lyr) > 0) {
    SpP_ras <- rasterize(SPDF, r3, getCover = T)
    SpP_ras[SpP_ras == 0] <- NA
    r4 <- mask(r3, SpP_ras) #___OPTION 1: all intersection between cells and polygons
    # r4 <- mask(r3, SPDF) #___OPTION 2: only cells with there centroids falling in polygons are accounted for
    
    summary$n_cells_overlap[i] = length(r4[r4 >= 1])
  } else {
    summary$n_cells_overlap[i] = NA
  }
  
  r_new[[i]] <- r4
  
  summary$layer[i] = d
  summary$n_cells_NCAR[i] = length(r3[r3 >= 1])
  
  i = i + 1
  
}

summary <- summary %>%
  mutate(month = str_extract(layer, "[A-z]+"),
         year = str_extract(layer, "[0-9]+"))

r_new <- do.call(brick, r_new)
r_new <- rast(r_new)
terra::writeRaster(r_new, "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2019.tiff", overwrite = T)

## Save summary
write.csv(summary, "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/summary_overlap_NCAR_OBS.csv", row.names = F)

#==================================================================
# BIMODAL BARPLOT BY MONTH => # CELLS
#==================================================================
rm(list=ls())

summary <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/summary_overlap_NCAR_OBS.csv")

summary_mean <- summary %>% 
  gather(n_cells_NCAR, n_cells_overlap, 2:3) %>%
  rename(n_cells = n_cells_overlap, type = n_cells_NCAR) %>%
  group_by(month, type) %>%
  summarise(n_cells = mean(n_cells))

summary_mean$month <- factor(summary_mean$month, levels = month.name)

n_cells_barplot <- ggplot(summary_mean, aes(x = month, fill = factor(type))) + 
  geom_bar(aes(y = n_cells), stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "H", "Cells") +
  xlab("Month")+
  ylab("Number of cells")+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.text = element_text(size = 10), #__________ legend texts
        legend.title = element_text(size = 17,face="bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )

ggsave("~/Dropbox/data/outputs_Marthe_2023/polynya contours/cells_overlap_barplot.png", height = 30, width = 30, units = c("cm"), dpi = 300)


#==================================================================
# OVERLAP ON ONE LAYER
#==================================================================
rm(list=ls())

d = "August.2004" #___sample date
source("~/Desktop/WHOI/Codes/useful_functions/df_to_SpatialPolygonsDataFrame.R")

r <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_monthly_polynya_2004_2019.tiff")
r_new <- raster::brick("~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/NCAR_polynya_cells_crop_polygons_2004_2019.tiff")
df_polynyas <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/all_contours_per_month/OBS_multiple_polynyas_contours_df.csv")

df_polynyas_lyr <- df_polynyas %>%
  mutate(new_id = paste0(ID, "-", num),
         date = paste0(month, ".", year)) %>%
  filter(date == d)

keep_poly <- df_polynyas_lyr %>%
  group_by(new_id) %>%
  summarise(n = n()) %>%
  filter(n > 4) %>%
  pull(new_id)

df_polynyas_lyr <- df_polynyas_lyr %>% filter(new_id %in% keep_poly)

SPDF <- df_to_SpatialPolygonsDataFrame(df_polynyas_lyr, split_polygons = "new_id")

e <- extent(SPDF)

pdf("~/Dropbox/data/outputs_Marthe_2023/polynya contours/example_overlap_NCAR_OBS.pdf", width = 7, height = 4)
print(plot(e, lwd = .0, col = "white", main = d))
print(plot(r$August.2004, add = T, col = "blue", 
           useRaster = FALSE, legend = F))
print(plot(r_new$August.2004, add = T, col = "green",
           useRaster = FALSE, legend = F))
print(plot(SPDF, add = T))
dev.off()

## End script
rm(list=ls())
