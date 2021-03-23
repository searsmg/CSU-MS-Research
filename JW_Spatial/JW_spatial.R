############################################
####### Joe Wright spatial analysis ########
############################################

library(rgdal)
library(gdalUtils)
library(raster)
library(sp)
library(streamstats)
library(sf)
library(rgeos)
library(ggplot2)


rm(list = ls())

  
# add DEM:
dem  <- raster("DEM/ned30m40105.tif")

# the distribution of values in the raster
hist(dem, main="Distribution of elevation values", 
     col= "purple", 
     maxpixels=22000000)

plot(dem, 
     main="DEM JW (need to clip)") # add title with main
cropbox1 <- drawExtent()

image(dem)

test <- delineateWatershed(xlocation = -105.882005, ylocation = 40.540477, crs = 4326,
                          includeparameters = "true")

leafletWatershed(JWboundary)

writeShapefile(watershed = test, layer = "layer_name", dir = "ws1_shp", what = "boundary")

JWboundary <- readOGR(dsn = "ws1_shp", layer = "layer_name")
JW_sensors <- readOGR(dsn="sensors", layer="BMsensors_all")

JWbound_proj <- spTransform(JWboundary,
                              crs(dem))
JWsens_proj <- spTransform(JW_sensors,
                            crs(dem))


## crop and mask
dem_crop <- crop(dem, extent(JWbound_proj))
JW_dem<- mask(dem_crop, JWbound_proj)

## Check that it worked
plot(JW_dem)
plot(JWsens_proj, add=TRUE)

hist(JW_dem, main="Distribution of elevation values", 
     col= "purple", 
     maxpixels=22000000)

contour <- contour(JW_dem, add=TRUE)
plot(JW_dem)+
  

# Plot the contours using spplot:
  spplot(contour_output["Elevation"],contour=TRUE)
