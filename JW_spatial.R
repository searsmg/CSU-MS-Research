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

# Setup an output filename (shapefile):
output_shapefile <- "input_dem.shp" #paste(tempfile(),".shp",sep="")

contour_output <- gdal_contour(src_filename=input_dem,dst_filename=input_dem,
                               a="Elevation",i=5.,output_Vector=TRUE)
# Plot the contours using spplot:
spplot(contour_output["Elevation"],contour=TRUE)
}
# }