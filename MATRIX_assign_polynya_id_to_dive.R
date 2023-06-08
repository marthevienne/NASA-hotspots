##
## Script name: assign_polynya_id_to_dive.R
##
## Purpose of script: Assign a polynya id to each dive (or 0 if dive does not occur in a polynya)
##                    as defined by NCAR (proxy = sea-ice concentration)
##
## Author: Marthe Vienne
##
## Date Created: 2023-04-25
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

## Functions
source("~/Desktop/WHOI/Codes/functions_polynyas/get_polynya_id_typical.R")

## Path input data
path_input = "~/Desktop/WHOI/Data/polynyas_contours/"

## Path output data
path_output = "~/Desktop/WHOI/Data/output_data/"

#==================================================================
# ASSIGN A POLYNYA ID TO EACH DIVE 
#==================================================================

dives = readRDS("~/Desktop/WHOI/Data/output_data/metrics_dives_V2")

# Mock dives
dives = data.frame(
  "lat" = c(-69, -77, -63.72815),
  "lon" = c(-2, 200,  -56.31249),
  "date" = c(
    as.POSIXct("03/09/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/05/1979", format = "%d/%m/%Y"),
    as.POSIXct("04/07/1979", format = "%d/%m/%Y")
  )
)

type = "CESM"
#type = "SSMI"

if (type == "CESM") {
  file_list_polynya_id = paste0(path_input, "CESM_polynya_typ_id")
} else {
  file_list_polynya_id = paste0(path_input, "SSMI_polynya_typ_id")
}

list_polynya = readRDS(file_list_polynya_id)

lon = list_polynya$lon
lat = list_polynya$lat
z = list_polynya$z

image(z)

if (type == "CESM") {
  dives$polynya_id_CESM = apply(dives, 1, function(dive)
    #get_polynya_id_typical(lon, lat, z, dive[25], dive[26]))
    get_polynya_id_typical(lon, lat, z, dive[2], dive[1]))
} else if (type == "SSMI") {
  dives$polynya_id_SSMI = apply(dives, 1, function(dive)
    #get_polynya_id_typical(lon, lat, z, dive[25], dive[26]))
    get_polynya_id_typical(lon, lat, z, dive[2], dive[1]))
}

saveRDS(dives, paste0(path_output, "metrics_dives_V3"))