##############
# Wpresence #
##############

# https://docs.google.com/file/d/0B50nlQZYf9okOElyd0o1M0Ewb28/edit

library(maps) # Commands for mapping.
library(mapdata) # Actual maps to draw from.
library(maptools)  # For working with shapefiles.
library(scales)  # To make lines transparent and adding scale bars.
library(mapproj) # For reprojecting maps and points.

jpeg("Map - Wpresence.jpg",height=800,width=1200,quality=100)

locations <- dataSPACE # Reads in the GPS data for your locations.
map(database= "state", "Connecticut", col="gray70", fill=TRUE, projection="azequalarea", resolution=0)
# Note that the ylim and xlim parameters tell the program how wide and tall to make the map; use col to change the fill color of the map;
# The projection parameter tells the program how to project the map; in this case we use an equal area map; 
# Orientation orients the map around a point (given by the first two numbers), and gives it a rotation (third number).
# Be sure to set the resolution to "0" so that the map is produced at the highest possible resolution.

# coord <- mapproject(locations$longitude, locations$latitude, proj="")  # Projects the points using the same projection used for the map.
# points(coord, pch=19, col="blue", cex=1) # See below.
# This past command plots the points and gives them a shape (pch) color (col) as well as a size (cex);
# Look here for pch numbers and their corresponding symbols: http://voteview.com/symbols_pch.htm;
# Change the color to any color you like ("red", etc); Increase the cex number to make points bigger;

locations <- cbind(dataSPACE,dataFP$wolffia)
colnames(locations)[3] <- "wolffia"

# Wolffia = 0
locations_FP0 <- subset(locations, locations$wolffia==0)
coord_FP0 <- mapproject(locations_FP0$longitude, locations_FP0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=19, col="white", cex=2) 

# Wolffia = 1
locations_W1 <- subset(locations, locations$wolffia==1)
coord_FP1 <- mapproject(locations_W1$longitude, locations_W1$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=19, col="darkgreen", cex=2) 

dev.off()
