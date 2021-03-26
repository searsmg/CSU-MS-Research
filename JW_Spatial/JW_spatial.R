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
dem  <- raster("JW_Spatial/DEM/ned30m40105.tif")

# the distribution of values in the raster
#hist(dem, main="Distribution of elevation values", 
#     col= "purple", 
#     maxpixels=22000000)

#plot(dem, 
#     main="DEM JW (need to clip)") # add title with main
#cropbox1 <- drawExtent()

#image(dem)

#test <- delineateWatershed(xlocation = -105.882005, ylocation = 40.540477, crs = 4326,
#                          includeparameters = "true")

#leafletWatershed(JWboundary)

#writeShapefile(watershed = test, layer = "layer_name", dir = "ws1_shp", what = "boundary")

## add in sensor and watershed boundary - NOTE to add outlet sp
JWboundary <- readOGR(dsn = "JW_Spatial/JW_boundary", layer = "globalwatershed")
JW_sensors <- readOGR(dsn="JW_Spatial/SensorLocations", layer="BMsensors_all")

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


hist(JW_dem,
     main=NULL,
     breaks=7,
     maxpixels=22000000, ylab="Frequency", xlab="Elevation (m)")

#Plot size
PlotWidth = 15
PlotHeight = 9

# use this plot format on remaining assignments - tweaked to remove grid lines and gray background
PlotFormat = theme(axis.text=element_text(size=20),
                   axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),              
                   axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_blank(),                                                                    
                   legend.text=element_text(size=20),                                                                   
                   legend.position = "bottom", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))


ggplot(demdata)+geom_histogram(aes(x=ned30m40105), fill="black", color="red", binwidth = 120) + PlotFormat

sum(JW_dem)