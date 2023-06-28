##
## Script name: NCAR_polynya_contours
##
## Purpose of script: Format polynya netcdf to raster 
##                    1) a. JRA-1p4 run | HI 0.4 threshold
##                    1) b. JRA-1p4 run | AICE 85% threshold
##                    2) POLYNYA RASTER TO DATAFRAME -> NECESSARY???
##                    // 3) a. Run 1 | HI 0.4 threshold -> old //
##                    // 3) b. SSMI | AICE 85% threshold -> old //
##
## Author: Marthe Vienne
##
## Date Created: 2023-06-02
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
## ---------------------------
## Library
library(ncdf4) # for netcdf manipulation
library(lubridate)
library(raster)
library(dplyr)
## ---------------------------
rm(list = ls())

## Functions
file.sources = list.files(path = "~/Desktop/WHOI/Codes/functions_raster/", pattern="*.R", full.names = T)
sapply(file.sources, source, .GlobalEnv)

#==================================================================
# 1) a. JRA-1p4 run | HI 0.4 threshold
#==================================================================

file = "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021"
sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

res <- netcdf_to_raster(
  file_name = paste0(file, ".nc"),
  lon_name = "tlon1d",
  lat_name = "tlat1d",
  var_name = "polynya_regions",
  n_years = 1,
  n_months = 12,
  tolerance = 0.0001
)

plot(res[[1]]) #___test plot

## Identify cell clusters => id not consistent across layers
# n_lyrs = res@data@nlayers
# res_id <- list()
# a <- 1
# for (lyr in 1:n_lyrs) {
#   res_id[[a]] <- identify_grouped_cells_raster(res[[lyr]])
#   a <- a + 1
# }
# res_id <- do.call(brick, res_id)
# names(res_id) <- month.name

names(res) <- month.name

file_output = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021.tif"
res2 <- rast(res)
terra::writeRaster(res2, file_output, overwrite = T)

# file_output_id = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021.tif"
# res_id2 <- rast(res_id)
# terra::writeRaster(res_id2, file_output_id, overwrite = T)

# raster::brick(filename) #____read raster stack

#==================================================================
# 1) b. JRA-1p4 run | AICE 85% threshold
#==================================================================

file = "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021"

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

res <- netcdf_to_raster(
  file_name = paste0(file, ".nc"),
  lon_name = "tlon1d",
  lat_name = "tlat1d",
  var_name = "polynya_regions",
  n_years = 1,
  n_months = 12,
  tolerance = 0.0001
)

plot(res[[1]])

## Identify cell clusters => id not consistent across layers
# n_lyrs = res@data@nlayers
# res_id <- list()
# a <- 1
# for (lyr in 1:n_lyrs) {
#   res_id[[a]] <- identify_grouped_cells_raster(res[[lyr]])
#   a <- a + 1
# }
# res_id <- do.call(brick, res_id)
# names(res_id) <- month.name

names(res) <- month.name

file_output = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021.tif"
res2 <- rast(res)
terra::writeRaster(res2, file_output, overwrite = T)

# file_output_id = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021.tif"
# res_id2 <- rast(res_id)
# terra::writeRaster(res_id2, file_output_id, overwrite = T)

# raster::brick(filename) #____example read raster stack

#==================================================================
# 2) POLYNYA RASTER TO DATAFRAME
#==================================================================
# filename = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021"
# filename = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021"
# filename = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021"
filename = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021"

r <- raster::brick(paste0(filename, ".tif")) #____read raster stack
df = raster_to_df(r)

write.csv(df, paste0(filename, "_df.csv"), row.names = F)

#==================================================================
# 3) a. Run 1 | HI 0.4 threshold
#==================================================================
file =  "~/Dropbox/data/polynya_contours_NCAR_2023/SSMI/jra55_typical_polynya_mask_hi_0.4mthresh" #___w/o extension

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

hi_typ <- netcdf_to_raster(
  file_name = paste0(file, ".nc"),
  lon_name = "TLONG_sub",
  lat_name = "TLAT_sub",
  var_name = "polynya_typical_all",
  n_years = 1,
  n_months = 1,
  tolerance = 0.0001
)

hi_typ_id <- identify_grouped_cells_raster(hi_typ)

plot(hi_typ_id)

#==================================================================
# 3) b. SSMI | AICE 85% threshold
#==================================================================
file =  "~/Dropbox/data/polynya_contours_NCAR_2023/SSMI/ssmi_cdr_typical_polynya_mask_85%thresh" #___w/o extension

sink(paste0(file, "_metadata.txt"))
print(nc_open(paste0(file, ".nc")))
sink()

SSMI_typ <- netcdf_to_raster(
  file_name = paste0(file, ".nc"),
  lon_name = "TLONG_sub",
  lat_name = "TLAT_sub",
  var_name = "polynya_typical_all",
  n_years = 1,
  n_months = 1,
  tolerance = 0.0001
)

SSMI_typ_id <- identify_grouped_cells_raster(SSMI_typ)

plot(SSMI_typ_id)

## End script
rm(list=ls())
