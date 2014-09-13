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
correlog_W_raw_incr20 <- correlog(data$latitude, data$longitude, data$wolffia_sp, latlon=TRUE, increment=20, resamp=500)
correlog_W_raw_incr20
plot(correlog_W_raw_incr20)

# Increment = 10
correlog_W_raw_incr10 <- correlog(data$latitude, data$longitude, data$wolffia_sp, latlon=TRUE, increment=10, resamp=500)
correlog_W_raw_incr10
plot(correlog_W_raw_incr10)

correlog_W_raw_incr10$correlation

# Increment = 2
correlog_W_raw_incr2 <- correlog(data$latitude, data$longitude, data$wolffia_sp, latlon=TRUE, increment=2, resamp=500)
correlog_W_raw_incr2
plot(correlog_W_raw_incr2)


#######################
# Moran's I           #
# Spatial correlogram #
# GLM resids          #
#######################
# Increment = 20
correlog_W_resid_incr20 <- correlog(data$latitude, data$longitude, best_glm_W_trans_resid, latlon=TRUE, increment=20, resamp=500)
correlog_W_resid_incr20
plot(correlog_W_resid_incr20)

# Increment = 10
correlog_W_resid_incr10 <- correlog(data$latitude, data$longitude, best_glm_W_trans_resid, latlon=TRUE, increment=10, resamp=500)
correlog_W_resid_incr10
plot(correlog_W_resid_incr10)

# Increment = 2
correlog_W_resid_incr2 <- correlog(data$latitude, data$longitude, best_glm_W_trans_resid, latlon=TRUE, increment=2, resamp=500)
correlog_W_resid_incr2
plot(correlog_W_resid_incr2)

#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 10
x <- seq(increment,200,increment)
y <- correlog_W_resid_incr10$correlation
p <- correlog_W_resid_incr10$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_W_raw_incr10$correlation
p <- correlog_W_raw_incr10$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[24] <- "Significant"

correlogram_W_incr10 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_W_incr10 <- correlogram_W_incr10 + geom_point(aes(shape=signif),size=3)
correlogram_W_incr10 <- correlogram_W_incr10 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_W_incr10 <- correlogram_W_incr10 + geom_line()
correlogram_W_incr10 <- correlogram_W_incr10 + xlab("Distance class (km)")
correlogram_W_incr10 <- correlogram_W_incr10 + ylab("Moran's I")
correlogram_W_incr10 <- correlogram_W_incr10 + ylim(-0.2,0.2)
correlogram_W_incr10 <- correlogram_W_incr10 + geom_hline(yintercept=0,linetype="longdash")
correlogram_W_incr10 <- correlogram_W_incr10 + ggtitle("Wolffia")
correlogram_W_incr10 <- correlogram_W_incr10 + scale_linetype(name="Data")
correlogram_W_incr10 <- correlogram_W_incr10 + theme_classic(base_size=18)
correlogram_W_incr10 <- correlogram_W_incr10 + theme(plot.title=element_text(face="italic")) 
correlogram_W_incr10 

ggsave("Correlogram - Wolffia - incr10.jpg",correlogram_W_incr10,height=8,width=11)



#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 20
x <- seq(increment,200,increment)
y <- correlog_W_resid_incr20$correlation
p <- correlog_W_resid_incr20$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_W_raw_incr20$correlation
p <- correlog_W_raw_incr20$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 

correlogram_W_incr20 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_W_incr20 <- correlogram_W_incr20 + geom_point(aes(shape=signif),size=3)
correlogram_W_incr20 <- correlogram_W_incr20 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_W_incr20 <- correlogram_W_incr20 + geom_line()
correlogram_W_incr20 <- correlogram_W_incr20 + xlab("Distance class (km)")
correlogram_W_incr20 <- correlogram_W_incr20 + ylab("Moran's I")
correlogram_W_incr20 <- correlogram_W_incr20 + ylim(-0.2,0.2)
correlogram_W_incr20 <- correlogram_W_incr20 + geom_hline(yintercept=0,linetype="longdash")
correlogram_W_incr20 <- correlogram_W_incr20 + ggtitle("Wolffia")
correlogram_W_incr20 <- correlogram_W_incr20 + scale_linetype(name="Data")
correlogram_W_incr20 <- correlogram_W_incr20 + theme_classic(base_size=18)
correlogram_W_incr20 <- correlogram_W_incr20 + theme(plot.title=element_text(face="italic")) 
correlogram_W_incr20 

ggsave("Correlogram - Wolffia - incr20.jpg",correlogram_W_incr20,height=8,width=11)
