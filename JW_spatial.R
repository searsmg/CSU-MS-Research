############################################
####### Joe Wright spatial analysis ########
############################################

library(rgdal)
library(gdalUtils)
library(raster)
library(sp)


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

