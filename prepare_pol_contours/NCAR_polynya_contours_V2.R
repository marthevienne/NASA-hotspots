##
## Script name: polynya_contours
##
## Purpose of script: 1) Open and rearrange polynya contours data from NCAR (netcdf) for SSMI and CESM data
##                        => create list object with lon, lat, z (NA or 1)
##                    2) Plot polynyas map
##                    3) Identify polynya
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
rm(list = ls())

## Library
library(ncdf4) # for netcdf manipulation
library(lubridate)
library(raster)
library(dplyr)

setwd("~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/JRA_4p2z_run/")

## Functions
file.sources = list.files(path = "~/Desktop/WHOI/Codes/functions_raster/", pattern="*.R", full.names = T)
sapply(file.sources, source, .GlobalEnv)

## Path input
path_input = "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/JRA_4p2z_run/"

## Path output data
path_output = "~/Desktop/WHOI/Data/polynyas_contours/NCAR/JRA_4p2z_run/"

## Path output figure
path_fig = ("~/Dropbox/data/outputs_Marthe_2023/polynya contours/")

#==================================================================
# 1) a. JRA-1p4 run | HI 0.4 threshold
#==================================================================

file =  paste0(path_input, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021")

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

n_lyrs = res@data@nlayers
res_id <- list()
a <- 1
for (lyr in 1:n_lyrs) {
  res_id[[a]] <- identify_grouped_cells_raster(res[[lyr]])
  a <- a + 1
}
res_id <- do.call(brick, res_id)

names(res) <- month.name
names(res_id) <- month.name

file_output = paste0(path_output, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021.tif")
res2 <- rast(res)
terra::writeRaster(res2, file_output, overwrite = T)

file_output_id = paste0(path_output, "ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021.tif")
res_id2 <- rast(res_id)
terra::writeRaster(res_id2, file_output_id, overwrite = T)

# raster::brick(filename) #____read raster stack

#==================================================================
# 1) b. JRA-1p4 run | AICE 85% threshold
#==================================================================

file =  paste0(path_input, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021")

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

n_lyrs = res@data@nlayers
res_id <- list()
a <- 1
for (lyr in 1:n_lyrs) {
  res_id[[a]] <- identify_grouped_cells_raster(res[[lyr]])
  a <- a + 1
}
res_id <- do.call(brick, res_id)

names(res) <- month.name
names(res_id) <- month.name

file_output = paste0(path_output, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021.tif")
res2 <- rast(res)
terra::writeRaster(res2, file_output, overwrite = T)

file_output_id = paste0(path_output, "ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021.tif")
res_id2 <- rast(res_id)
terra::writeRaster(res_id2, file_output_id, overwrite = T)

# raster::brick(filename) #____read raster stack

#==================================================================
# 2) POLYNYA DATA TO DATAFRAME
#==================================================================
# filename = paste0(path_output, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021")
# filename = paste0(path_output, "ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.aice_85%thresh.polynya_sh.2004-2021")
# filename = paste0(path_output, "g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021")
filename = paste0(path_output, "ID_g.e22.GOMIPECOIAF_JRA-1p4-2018.TL319_g17.4p2z.001branch.monthly_clim.coastal_polynyas.hi_0.4mthresh.polynya_sh.2004-2021")

r <- raster::brick(paste0(filename, ".tif")) #____read raster stack
df = raster_to_df(r)

write.csv(df, paste0(filename, "_df.csv"), row.names = F)

#==================================================================
# 3) a. Run 1 | HI 0.4 threshold
#==================================================================
path_input = "~/Dropbox/data/polynya_contours_NCAR_2023/Low res run/run 1/"

file =  paste0(path_input, "jra55_typical_polynya_mask_hi_0.4mthresh")

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
path_input = "~/Dropbox/data/polynya_contours_NCAR_2023/SSMI/"

file =  paste0(path_input, "ssmi_cdr_typical_polynya_mask_85%thresh")

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
