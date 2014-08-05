library(maptools) 
library(sp)
library(rgdal)
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables
library(mapproj) # For reprojecting maps and points.
# I may need to load more packages 


# change your WD 
setwd("C:/Users/Mike/Desktop/Dropbox/absences/GIS")
setwd("C:/Users/Mike/Desktop/Dropbox/absences")

getinfo.shape("CT_VICINITY_STATE_POLY.shp") 

# read in the Connecticut base map 
CT_base_map<-readShapePoly("CT_VICINITY_STATE_POLY.shp") 

#########################
# Define the projection #
#########################
# look up the metadata:
# http://www.cteco.uconn.edu/metadata/dep/document/CT_VICINITY_STATE_POLY_FGDC_Plus.htm
# Projected coordinate system:
# Name: NAD 1983 StatePlane Connecticut FIPS 0600 Feet
# Map units: survey feet

# get the coordinate reference system
# search for the name of the projected coorindate system here: http://www.spatialreference.org/
# choose the "proj4" 
proj4string(CT_base_map)<-CRS("+proj=lcc +lat_1=41.2 +lat_2=41.86666666666667 +lat_0=40.83333333333334 +lon_0=-72.75 +x_0=304800.6096 +y_0=152400.3048 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs") 

# transform the projection  
CT_base_map<-spTransform(CT_base_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 

plot(CT_base_map)

#######################################################
# build a data frame to hold FPrichness and locations # 
#######################################################
dataSPACE_SPECIES <- cbind(dataSPACE,
                          data$FP_species_richness,
                          data$lemna_minor, 
                          data$lemna_trisulca,
                          data$spirodela_polyrhiza,
                          dataFP$wolffia)

colnames(dataSPACE_SPECIES)[3] <- "FP_species_richness"
colnames(dataSPACE_SPECIES)[4] <- "lemna_minor"
colnames(dataSPACE_SPECIES)[5] <- "lemna_trisulca"
colnames(dataSPACE_SPECIES)[6] <- "spirodela_polyrhiza"
colnames(dataSPACE_SPECIES)[7] <- "wolffia"

head(dataSPACE_SPECIES)

# this data frame should hold all of the attributes that I care about 

##################################
# Add the points to the base map #
# Trying to be fancy about it    #
# AUtomatically find colors      #
# based on breaks in resp. var.  #
# not good for integer resp.     #
##################################
points(dataSPACE_SPECIES$longitude,
       dataSPACE_SPECIES$latitude,
       pch=19,
       col="red",
       cex=1)
# this just plots the points, it does not give the attribute (FP richness)

plotvar <- dataSPACE_SPECIES$FP_species_richness
nclr <- length(unique(dataSPACE_SPECIES$FP_species_richness))
plotclr <- brewer.pal(nclr,"Greens")
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)

points(dataSPACE_SPECIES$longitude, dataSPACE_SPECIES$latitude, pch=16, col=plotclr, cex=1)
points(dataSPACE_SPECIES$longitude, dataSPACE_SPECIES$latitude, cex=1)

legend(-71, 42, 
       legend=names(attr(colcode, "table")), 
       fill=attr(colcode, "palette"), cex=0.6, bty="n")

###############
# FP richness #
###############
jpeg("GIS - FPrichness.jpg",height=800,width=1200,quality=100)

plot(CT_base_map)

dataSPACE_SPECIES_FP0 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$FP_species_richness==0)
coord_FP0 <- mapproject(dataSPACE_SPECIES_FP0$longitude, dataSPACE_SPECIES_FP0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=16, col="white", cex=2) 
points(coord_FP0, cex=2) 

# FP richness = 1
dataSPACE_SPECIES_FP1 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$FP_species_richness==1)
coord_FP1 <- mapproject(dataSPACE_SPECIES_FP1$longitude, dataSPACE_SPECIES_FP1$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=16, col="darkolivegreen1", cex=2) 
points(coord_FP1, cex=2) 

# FP richness = 2
dataSPACE_SPECIES_FP2 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$FP_species_richness==2)
coord_FP2 <- mapproject(dataSPACE_SPECIES_FP2$longitude, dataSPACE_SPECIES_FP2$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP2, pch=16, col="darkolivegreen4", cex=2) 
points(coord_FP2, cex=2) 

# FP richness = 3
dataSPACE_SPECIES_FP3 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$FP_species_richness==3)
coord_FP3 <- mapproject(dataSPACE_SPECIES_FP3$longitude, dataSPACE_SPECIES_FP3$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP3, pch=16, col="darkolivegreen", cex=2) 
points(coord_FP3, cex=2) 

# FP richness = 4
dataSPACE_SPECIES_FP4 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$FP_species_richness==4)
coord_FP4 <- mapproject(dataSPACE_SPECIES_FP4$longitude, dataSPACE_SPECIES_FP4$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP4, pch=16, col="darkgreen", cex=2) 
points(coord_FP4, cex=2) 

dev.off()

###############
# lemna_minor #
###############
jpeg("GIS - lemna_minor.jpg",height=800,width=1200,quality=100)

plot(CT_base_map)

dataSPACE_SPECIES_lemna_minor0 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$lemna_minor==0)
coord_FP0 <- mapproject(dataSPACE_SPECIES_lemna_minor0$longitude, dataSPACE_SPECIES_lemna_minor0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=16, col="white", cex=2) 
points(coord_FP0, cex=2) 

# FP richness = 1
dataSPACE_SPECIES_lemna_minor <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$lemna_minor==1)
coord_FP1 <- mapproject(dataSPACE_SPECIES_lemna_minor$longitude, dataSPACE_SPECIES_lemna_minor$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=16, col="darkgreen", cex=2) 
points(coord_FP1, cex=2) 

dev.off()


#######################
# spirodela_polyrhiza #
#######################
jpeg("GIS - spirodela_polyrhiza.jpg",height=800,width=1200,quality=100)

plot(CT_base_map)

dataSPACE_SPECIES_spirodela_polyrhiza0 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$spirodela_polyrhiza==0)
coord_FP0 <- mapproject(dataSPACE_SPECIES_spirodela_polyrhiza0$longitude, dataSPACE_SPECIES_spirodela_polyrhiza0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=16, col="white", cex=2) 
points(coord_FP0, cex=2) 

# FP richness = 1
dataSPACE_SPECIES_spirodela_polyrhiza <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$spirodela_polyrhiza==1)
coord_FP1 <- mapproject(dataSPACE_SPECIES_spirodela_polyrhiza$longitude, dataSPACE_SPECIES_spirodela_polyrhiza$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=16, col="darkgreen", cex=2) 
points(coord_FP1, cex=2) 

dev.off()

###########
# wolffia #
########### 
jpeg("GIS - wolffia.jpg",height=800,width=1200,quality=100)

plot(CT_base_map)

dataSPACE_SPECIES_wolffia0 <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$wolffia==0)
coord_FP0 <- mapproject(dataSPACE_SPECIES_wolffia0$longitude, dataSPACE_SPECIES_wolffia0$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP0, pch=16, col="white", cex=2) 
points(coord_FP0, cex=2) 

# FP richness = 1
dataSPACE_SPECIES_wolffia <- subset(dataSPACE_SPECIES, dataSPACE_SPECIES$wolffia==1)
coord_FP1 <- mapproject(dataSPACE_SPECIES_wolffia$longitude, dataSPACE_SPECIES_wolffia$latitude, proj="")  # Projects the points using the same projection used for the map.
points(coord_FP1, pch=16, col="darkgreen", cex=2) 
points(coord_FP1, cex=2) 

dev.off()

