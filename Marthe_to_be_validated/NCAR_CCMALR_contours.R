path_output = ("~/Dropbox/data/outputs_Marthe_2023/")

setwd("~/Dropbox/data/polynya_contours_NCAR_2023/")
file3 <- "CESM_gx1v7_CCAMLR_masks_v3_01032023"

nc_CESM_polynyas <- nc_open(paste0(file3, ".nc"))

lat <-
  ncvar_get(nc_CESM_polynyas, "lat2d")[1,] # code = 0 (other) or 1 (polynya)

lon <-
  ncvar_get(nc_CESM_polynyas, "lon2d_180")[,1] # code = 0 (other) or 1 (polynya)

res_grid <-
  ncvar_get(nc_CESM_polynyas, "tarea") # code = 0 (other) or 1 (polynya)
#res grid not constant along latitude gradient -> plus large au niveau des tropiques

lon[which(lon > 180)] = lon[which(lon > 180)] - 360

(max(lat) - min(lat)) / 384
(max(lon) - min(lon)) / 320

image(y = lat, res_grid)

plot(lon)

toto <-
  ncvar_get(nc_CESM_polynyas, "coast_mask")

pdf(
  paste0(path_output, "test.pdf"),
  height = 5,
  width = 5
)
print(image(y = lat, toto))
dev.off()

##########
library(raster)
x <- raster(nrow=384, ncol=320, ymn = -79.22052, ymx = 72.18598, xmn = -178.9375, xmx = 179.9375)

seg_1 = c(which(lon == min(lon)):nrow(t(toto)))
seg_2 = seq(1, which(lon == min(lon)) -1)

values(x) = t(toto)[c(seg_1, seg_2),]

#### PB LON
e <- as(extent(-178.9375, 179.9375, -79.22052, -58), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(x, e)
plot(x)
#axis(side=1, at = seq(-178.9375, 179.9375, length.out = length(lon)), labels=lon)

