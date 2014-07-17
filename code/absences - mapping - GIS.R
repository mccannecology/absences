library(maptools) 
library(sp)
library(rgdal)
# I may need to load more packages 


# change your WD 
setwd("C:/Users/Mike/Desktop/Dropbox/absences/GIS")

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


#######################################################
# build a data frame to hold FPrichness and locations # 
#######################################################
dataSPACE_FPrich <- cbind(dataSPACE,data$FP_species_richness)

# this data frame should hold all of the attributes that I care about 

##################################
# Add the points to the base map #
##################################
points(dataSPACE_FPrich$longitude,
       dataSPACE_FPrich$latitude,
       pch=19,
       col="red",
       cex=1)

# this just plots the points, it does not give the attribute (FP richness)


