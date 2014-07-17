##############
# FPrichness #
##############

# https://docs.google.com/file/d/0B50nlQZYf9okOElyd0o1M0Ewb28/edit

library(maps) # Commands for mapping.
library(mapdata) # Actual maps to draw from.
library(maptools)  # For working with shapefiles.
library(scales)  # To make lines transparent and adding scale bars.
library(mapproj) # For reprojecting maps and points.

jpeg("Map - FPrichness.jpg",height=800,width=1200,quality=100)

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

locations <- cbind(dataSPACE,data$FP_species_richness)
colnames(locations)[3] <- "FP_species_richness"

# FP richness = 0
locations_FP0 <- subset(locations, locations$FP_species_richness==0)
coord_FP0 <- mapproject(locations_FP0$longitude, locations_FP0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=19, col="white", cex=2) 

# FP richness = 1
locations_FP1 <- subset(locations, locations$FP_species_richness==1)
coord_FP1 <- mapproject(locations_FP1$longitude, locations_FP1$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=19, col="darkolivegreen1", cex=2) 

# FP richness = 2
locations_FP2 <- subset(locations, locations$FP_species_richness==2)
coord_FP2 <- mapproject(locations_FP2$longitude, locations_FP2$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP2, pch=19, col="darkolivegreen4", cex=2) 

# FP richness = 3
locations_FP3 <- subset(locations, locations$FP_species_richness==3)
coord_FP3 <- mapproject(locations_FP3$longitude, locations_FP3$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP3, pch=19, col="darkolivegreen", cex=2) 

# FP richness = 4
locations_FP4 <- subset(locations, locations$FP_species_richness==4)
coord_FP4 <- mapproject(locations_FP4$longitude, locations_FP4$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP4, pch=19, col="darkgreen", cex=2) 

dev.off()
