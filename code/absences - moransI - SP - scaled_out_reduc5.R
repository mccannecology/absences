library(ncf)
library(ggplot2)

load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5.RData")
# or 
load(("C:/Users/Mike/Desktop/Dropbox/absences/workspace - morans i - scaled_out - reduced5.RData"))


#######################
# Moran's I           #
# Spatial correlogram #
# Raw data            #
#######################
# Increment = 20
correlog_SP_raw_incr20 <- correlog(data$latitude, data$longitude, data$spirodela_polyrhiza, latlon=TRUE, increment=20, resamp=500)
correlog_SP_raw_incr20
plot(correlog_SP_raw_incr20)

# Increment = 10
correlog_SP_raw_incr10 <- correlog(data$latitude, data$longitude, data$spirodela_polyrhiza, latlon=TRUE, increment=10, resamp=500)
correlog_SP_raw_incr10
plot(correlog_SP_raw_incr10)

# Increment = 5
correlog_SP_raw_incr5 <- correlog(data$latitude, data$longitude, data$spirodela_polyrhiza, latlon=TRUE, increment=5, resamp=500)
correlog_SP_raw_incr5
plot(correlog_SP_raw_incr5)

# Increment = 2
correlog_SP_raw_incr2 <- correlog(data$latitude, data$longitude, data$spirodela_polyrhiza, latlon=TRUE, increment=2, resamp=500)
correlog_SP_raw_incr2
plot(correlog_SP_raw_incr2)

#######################
# Moran's I           #
# Spatial correlogram #
# SP resids       #
#######################
# Increment = 20
correlog_SP_resid_incr20 <- correlog(data$latitude, data$longitude, best_glm_SP_scaled_out_reduc5_resid, latlon=TRUE, increment=20, resamp=500)
correlog_SP_resid_incr20
plot(correlog_SP_resid_incr20)

# Increment = 10
correlog_SP_resid_incr10 <- correlog(data$latitude, data$longitude, best_glm_SP_scaled_out_reduc5_resid, latlon=TRUE, increment=10, resamp=500)
correlog_SP_resid_incr10
plot(correlog_SP_resid_incr10)

# Increment = 5
correlog_SP_resid_incr5 <- correlog(data$latitude, data$longitude, best_glm_SP_scaled_out_reduc5_resid, latlon=TRUE, increment=5, resamp=500)
correlog_SP_resid_incr5
plot(correlog_SP_resid_incr5)

# Increment = 2
correlog_SP_resid_incr2 <- correlog(data$latitude, data$longitude, best_glm_SP_scaled_out_reduc5_resid, latlon=TRUE, increment=2, resamp=500)
correlog_SP_resid_incr2
plot(correlog_SP_resid_incr2)

#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 10
x <- seq(increment,70,increment)
y <- correlog_SP_resid_incr10$correlation[1:length(x)]
p <- correlog_SP_resid_incr10$p[1:length(x)]
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_SP_raw_incr10$correlation[1:length(x)]
p <- correlog_SP_raw_incr10$p[1:length(x)]
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[data_temp$p <= 0.05] <- "Significant" 

correlogram_SP_incr10 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_point(aes(shape=signif),size=3)
correlogram_SP_incr10 <- correlogram_SP_incr10 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_line()
correlogram_SP_incr10 <- correlogram_SP_incr10 + xlab("Distance class (km)")
correlogram_SP_incr10 <- correlogram_SP_incr10 + ylab("Moran's I")
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_hline(yintercept=0,linetype="longdash")
correlogram_SP_incr10 <- correlogram_SP_incr10 + ggtitle("Spirodela polyrhiza")
correlogram_SP_incr10 <- correlogram_SP_incr10 + scale_linetype(name="Data")
correlogram_SP_incr10 <- correlogram_SP_incr10 + theme_classic(base_size=18)
correlogram_SP_incr10 <- correlogram_SP_incr10 + theme(plot.title=element_text(face="italic")) 
correlogram_SP_incr10 

ggsave("Correlogram - Spirodela polyrhiza - incr10 - scaled_out_reduc5.jpg",correlogram_SP_incr10,height=8,width=11)


################################
# Cobmine all species together #
################################

######## 
# Save # 
########
save.image("C:/Users/Mike/Desktop/Dropbox/absences/workspace - morans i - scaled_out - reduced5.RData")
