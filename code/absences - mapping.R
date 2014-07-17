head(dataFP)
head(dataSPACE)

###############################################
# CT base map 
###############################################
# http://www.ct.gov/deep/cwp/view.asp?a=2698&q=322898&deepNav_GID=1707%20
# http://www.spatialreference.org/

library(ctv) 
library(maps)
library(maptools) 
library(rgdal) 
library(spdep) 

getinfo.shape("CT_VICINITY_STATE_POLY.shp") 

map<-readShapePoly("CT_VICINITY_STATE_POLY.shp") 

# http://www.nyu.edu/projects/politicsdatalab/workshops/GISwR.pdf

map("state","Connecticut")
points<-dataSPACE
plot(points$longitude,points$latitude,pch=19)

class(map)
summary(map)

# determine the projection by looking up the metadata 
# searching for the name of the projected coorindate system here: http://www.spatialreference.org/
# choose the "proj4" 
proj4string(map)<-CRS("+proj=lcc +lat_1=41.2 +lat_2=41.86666666666667 +lat_0=40.83333333333334 +lon_0=-72.75 +x_0=304800.6096 +y_0=152400.3048 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs") 
map<-spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 


crs.geo <- CRS("+proj=lcc +lat_1=41.2 +lat_2=41.86666666666667 +lat_0=40.83333333333334 +lon_0=-72.75 +x_0=304800.6096 +y_0=152400.3048 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs") 
proj4string(dataSPATIAL) <- crs.geo  # define projection system of our data
summary(dataSPATIAL)

plot(map)

plot(map,dataSPATIAL,add=T)



###############################################
# Examples with different packges 
###############################################
library(rworldmap)
library(dismo)
library(googleVis)

dataSPATIAL <- cbind(dataSPACE, data$FP_species_richness)
colnames(dataSPATIAL)[3] <- "FP_species_richness"
head(dataSPATIAL)

coordinates(dataSPATIAL) <- c("longitude", "latitude")  # set spatial coordinates
plot(dataSPATIAL)

# Define spatial projection
# Consult the appropriate PROJ.4 description here: http://www.spatialreference.org/
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(dataSPATIAL) <- crs.geo  # define projection system of our data
summary(dataSPATIAL)

# Quickly plotting point data on a map
plot(dataSPATIAL, pch = 20, col = "steelblue")

# this looks shitty 
data(coastsCoarse)
plot(coastsCoarse, add = T)

# or this looks a little better
data(countriesLow)
plot(countriesLow, add = T)

summary(dataSPATIAL)


# Mapping vectorial data using gmap from dismo
map <- gmap(dataSPATIAL, type = "satellite")
dataSPATIAL.merc <- Mercator(dataSPATIAL)  # Google Maps are in Mercator projection. 
# This function projects the points to that projection to enable mapping
plot(map)

points(dataSPATIAL.merc, pch = 20, col = "red")


# Map vectorial data with googleVis (internet)
points <- as.data.frame(dataSPATIAL)
colnames(points)[1] <- "longitude" 
colnames(points)[2] <- "latitude"
points$latlon <- paste(points$latitude, points$longitude, sep=":")
map <- gvisMap(points, 
               locationvar="latlon", 
               options = list(showTip=T, showLine=F, enableScrollWheel=TRUE, useMapTypeControl=T, width=1400,height=1400))
plot(map)


