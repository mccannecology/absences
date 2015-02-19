library(ncf)
library(ggplot2)

head(data)
colnames(data)
dist(dataSPACE) # not sure if I actually need this 

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

correlog_SP_raw_incr10$correlation

# Increment = 2
correlog_SP_raw_incr2 <- correlog(data$latitude, data$longitude, data$spirodela_polyrhiza, latlon=TRUE, increment=2, resamp=500)
correlog_SP_raw_incr2
plot(correlog_SP_raw_incr2)


#######################
# Moran's I           #
# Spatial correlogram #
# GLM resids          #
#######################
# Increment = 20
correlog_SP_resid_incr20 <- correlog(data$latitude, data$longitude, best_glm_SP_trans_resid, latlon=TRUE, increment=20, resamp=500)
correlog_SP_resid_incr20
plot(correlog_SP_resid_incr20)

# Increment = 10
correlog_SP_resid_incr10 <- correlog(data$latitude, data$longitude, best_glm_SP_trans_resid, latlon=TRUE, increment=10, resamp=500)
correlog_SP_resid_incr10
plot(correlog_SP_resid_incr10)

# Increment = 2
correlog_SP_resid_incr2 <- correlog(data$latitude, data$longitude, best_glm_SP_trans_resid, latlon=TRUE, increment=2, resamp=500)
correlog_SP_resid_incr2
plot(correlog_SP_resid_incr2)

#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 10
x <- seq(increment,200,increment)
y <- correlog_SP_resid_incr10$correlation
p <- correlog_SP_resid_incr10$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_SP_raw_incr10$correlation
p <- correlog_SP_raw_incr10$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
# I *think* the significance levels is at 0.025 - (two-tailed)
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[8] <- "Significant"
data_temp$signif[9] <- "Significant"
data_temp$signif[27] <- "Significant"
data_temp$signif[28] <- "Significant"
data_temp$signif[32] <- "Significant"


correlogram_SP_incr10 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_point(aes(shape=signif),size=3)
correlogram_SP_incr10 <- correlogram_SP_incr10 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_line()
correlogram_SP_incr10 <- correlogram_SP_incr10 + xlab("Distance class (km)")
correlogram_SP_incr10 <- correlogram_SP_incr10 + ylab("Moran's I")
correlogram_SP_incr10 <- correlogram_SP_incr10 + ylim(-0.25,0.25)
correlogram_SP_incr10 <- correlogram_SP_incr10 + geom_hline(yintercept=0,linetype="longdash")
correlogram_SP_incr10 <- correlogram_SP_incr10 + ggtitle("Spirodela polyrhiza")
correlogram_SP_incr10 <- correlogram_SP_incr10 + scale_linetype(name="Data")
correlogram_SP_incr10 <- correlogram_SP_incr10 + theme_classic(base_size=18)
correlogram_SP_incr10 <- correlogram_SP_incr10 + theme(plot.title=element_text(face="italic")) 
correlogram_SP_incr10 

ggsave("Correlogram - Spirodela polyrhiza - incr10.jpg",correlogram_SP_incr10,height=8,width=11)


############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 20
x <- seq(increment,200,increment)
y <- correlog_SP_resid_incr20$correlation
p <- correlog_SP_resid_incr20$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_SP_raw_incr20$correlation
p <- correlog_SP_raw_incr20$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[4] <- "Significant"
data_temp$signif[5] <- "Significant"
data_temp$signif[11] <- "Significant"
data_temp$signif[12] <- "Significant"
data_temp$signif[14] <- "Significant"
data_temp$signif[15] <- "Significant"

correlogram_SP_incr20 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_SP_incr20 <- correlogram_SP_incr20 + geom_point(aes(shape=signif),size=3)
correlogram_SP_incr20 <- correlogram_SP_incr20 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_SP_incr20 <- correlogram_SP_incr20 + geom_line()
correlogram_SP_incr20 <- correlogram_SP_incr20 + xlab("Distance class (km)")
correlogram_SP_incr20 <- correlogram_SP_incr20 + ylab("Moran's I")
correlogram_SP_incr20 <- correlogram_SP_incr20 + ylim(-0.25,0.25)
correlogram_SP_incr20 <- correlogram_SP_incr20 + geom_hline(yintercept=0,linetype="longdash")
correlogram_SP_incr20 <- correlogram_SP_incr20 + ggtitle("Spirodela polyrhiza")
correlogram_SP_incr20 <- correlogram_SP_incr20 + scale_linetype(name="Data")
correlogram_SP_incr20 <- correlogram_SP_incr20 + theme_classic(base_size=18)
correlogram_SP_incr20 <- correlogram_SP_incr20 + theme(plot.title=element_text(face="italic")) 
correlogram_SP_incr20 

ggsave("Correlogram - Spirodela polyrhiza - incr20.jpg",correlogram_SP_incr20,height=8,width=11)

