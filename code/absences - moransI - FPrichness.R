install.packages("fields")
library(ncf)

head(data)
dist(dataSPACE)

# Convert Lat Lon distances to km
library(fields)
dist_km <- as.matrix(rdist.earth(cbind(data$longitude, data$latitude), miles = F, R = NULL))
diag(dist_km) <- 0
dist_km

#############
# Moran's I #
# Raw data  #
#############

#######################
# Spatial correlogram #
# Raw data            #
#######################
correlog_FPrich_raw <- correlog(data$latitude, data$longitude, data$FP_species_richness, latlon=TRUE, increment=20, resamp=500)
correlog_FPrich_raw
plot(correlog_FPrich_raw)

##############
# Moran's I  #
# GLM resids #
##############

#######################
# Spatial correlogram #
# GLM resids          #
#######################
correlog_FPrich_resid <- correlog(data$latitude, data$longitude, glm_poisson_FPrich_best_resid, latlon=TRUE, increment=20, resamp=500)
correlog_FPrich_resid
plot(correlog_FPrich_resid)
