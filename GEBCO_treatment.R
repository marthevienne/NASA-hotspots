library(raster)
library(marmap)
library(ggfortify)

setwd("~/Desktop/WHOI/Data/bathy_data/")


## Schakleton and CP polynyas

# 2021
topo_SP_CP_2021 <- raster("~/Dropbox/data/bathymetry/bathy_Lucie_pol9_14.tif")
topo_SP_CP_2021

# 2023
topo_SP_CP_2023 <- raster("GEBCO_18_May_2023_07d3fced17ba/gebco_2023_n-63.0_s-70.0_w60.0_e110.0.tif")
topo_SP_CP_2023

topo_SP_CP_2023[topo_SP_CP_2023 >= -1] <- NA

## East Antarctica (GEBCO 2023)
topo_EA <- raster("GEBCO_18_May_2023_3496bb438f45/gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.tif")
topo_EA
topo_EA[topo_EA >= 0] <- NA
writeRaster(topo_EA, "HR_0.0041_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd", overwrite = T)

topo_EA_LR = aggregate(topo_EA, fact = 10)
topo_EA_LR[topo_EA_LR >= 0] <- NA
writeRaster(topo_EA_LR, "LR_0.041_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd", overwrite = T)

topo_EA_0.041 <- raster("LR_0.041_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")
topo_EA_LR = aggregate(topo_EA_0.041, fact = 2)
writeRaster(topo_EA_LR, "LR_0.083_TOPO_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd", overwrite = T)

bathy_EA = as.bathy(topo_EA_LR)
writeRaster(bathy_EA, "LR_BATHY_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd", overwrite = T)

##########
png("SP_bathy_zoom_2023.png", height = 1000, width = 1500, res = 300, units = "px")
plot(bathy, xlim = c(64, 105), ylim = c(-70, -63))
dev.off()
###########

### PLOT BATHY
source("~/Desktop/WHOI/Codes/palettes/palette_bathy.R")
bathy_EA <- raster("BATHY_gebco_2023_n-58.0_s-74.0_w-5.0_e170.0.grd")

autoplot.bathy(bathy_EA, geom=c("raster"), coast=TRUE, 
               mapping = aes(levels(bins = 100))) + 
  scale_fill_gradientn(colours = newcol)

ggsave("bathy_EA.png", height = 42, width = 40, units = c("cm"), dpi = 300)
