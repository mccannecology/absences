library(fields)
library(ncf)
library(ggplot2)

head(data)
dist(dataSPACE)

# Convert Lat Lon distances to km
library(fields)
dist_km <- as.matrix(rdist.earth(cbind(data$longitude, data$latitude), miles = F, R = NULL))
diag(dist_km) <- 0
# the distance matrix between all water bodies 
dist_km 

#######################
# Moran's I           #
# Spatial correlogram #
# Raw data            #
#######################
# Increment = 20
correlog_FPrich_raw_incr20 <- correlog(data$latitude, data$longitude, data$FP_species_richness, latlon=TRUE, increment=20, resamp=500)
correlog_FPrich_raw_incr20
plot(correlog_FPrich_raw_incr20)

# Increment = 10
correlog_FPrich_raw_incr10 <- correlog(data$latitude, data$longitude, data$FP_species_richness, latlon=TRUE, increment=10, resamp=500)
correlog_FPrich_raw_incr10
plot(correlog_FPrich_raw_incr10)

correlog_FPrich_raw_incr10$correlation

# Increment = 2
correlog_FPrich_raw_incr2 <- correlog(data$latitude, data$longitude, data$FP_species_richness, latlon=TRUE, increment=2, resamp=500)
correlog_FPrich_raw_incr2
plot(correlog_FPrich_raw_incr2)


#######################
# Moran's I           #
# Spatial correlogram #
# GLM resids          #
#######################
# Increment = 20
correlog_FPrich_resid_incr20 <- correlog(data$latitude, data$longitude, glm_poisson_FPrich_best_resid, latlon=TRUE, increment=20, resamp=500)
correlog_FPrich_resid_incr20
plot(correlog_FPrich_resid_incr20)

# Increment = 10
correlog_FPrich_resid_incr10 <- correlog(data$latitude, data$longitude, glm_poisson_FPrich_best_resid, latlon=TRUE, increment=10, resamp=500)
correlog_FPrich_resid_incr10
plot(correlog_FPrich_resid_incr10)

# Increment = 2
correlog_FPrich_resid_incr2 <- correlog(data$latitude, data$longitude, glm_poisson_FPrich_best_resid, latlon=TRUE, increment=2, resamp=500)
correlog_FPrich_resid_incr2
plot(correlog_FPrich_resid_incr2)

#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 10
x <- seq(increment,200,increment)
y <- correlog_FPrich_resid_incr10$correlation
p <- correlog_FPrich_resid_incr10$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_FPrich_raw_incr10$correlation
p <- correlog_FPrich_raw_incr10$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

data_temp$signif <- ifelse(data_temp$p < 0.05, "Significant","Not significant")

correlogram_FPrich__incr10 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + geom_point(aes(shape=signif),size=3)
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + geom_line()
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + xlab("Distance class")
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + ylab("Moran's I")
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + scale_linetype(name="Data")
correlogram_FPrich__incr10 <- correlogram_FPrich__incr10 + theme_classic(base_size=18)
correlogram_FPrich__incr10 

ggsave("Correlogram - FP_richness - incr10.jpg",correlogram_FPrich_incr10,height=8,width=11)
