## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-30
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
## ---------------------------
## Library
## ---------------------------
## Paths
## ---------------------------
## Functions
library(raster)
library(ncdf4)
library(RNetCDF)
## ---------------------------

# SpatRaster
r <- rast("~/Desktop/WHOI/Data/output_data/RES_0.2_residence_time_season.tif")
ncfname <- "~/Desktop/WHOI/Data/output_data/residence_time/RES_0.2_time_spent.nc"
names(r)
r <- subset(r, grep('SUM', names(r), value = T)) #___only total time spent per pixel (== SUM)
names(r)

r[is.na(r)] = -999 #___fill value netcdf

# Raster to 3D array (x, y, z)
vals <- rev(values(r)) #___values from raster and matrix are symetric following x axis
values <- array(vals, dim = c(ncol(r), nrow(r), 4))
rotate_val <-  values[c(nrow(values):1),, ]

# TODO: raster_to_netcdf()

# path and file name, set dname
dname <- "tot_time_spent"  # note: tmp means temperature (not temporary)

# create and write the netCDF file -- ncdf4 version
# define dimensions
lon <- seq(ext(r)[1] + res(r)[1]/2, ext(r)[2] - res(r)[1]/2, res(r)[1])
nlon <- length(lon)
lat <- seq(ext(r)[3] + res(r)[2]/2, ext(r)[4] - res(r)[2]/2, res(r)[2])
nlat <- length(lat)
londim <- ncdim_def( "lon", "degrees_east", lon)
latdim <- ncdim_def( "lat", "degrees_north", lat)
tdim <- ncdim_def( "seasons", "", 1:4, longname =  c("summer, autumn, winter, spring"))

# define variables
fillvalue <- -999
dlname <- "total time spent"
tmp_def <- ncvar_def("tot_time_spent", "days", list(londim, latdim, tdim), fillvalue, dlname, prec="single")

# create netcdf file ---> delete file first in directory
if (file.exists(ncfname)) {
  unlink(ncfname)
}
nc_out <- nc_create(ncfname, list(tmp_def), force_v4 = TRUE, verbose = F)
ncvar_put(nc_out, tmp_def, rotate_val)

## End script
rm(list=ls())
