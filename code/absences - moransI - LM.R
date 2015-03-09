library(ncf)
library(ggplot2)

load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5.RData")

#######################
# Moran's I           #
# Spatial correlogram #
# Raw data            #
#######################
# Increment = 20
correlog_LM_raw_incr20 <- correlog(data$latitude, data$longitude, data$lemna_minor, latlon=TRUE, increment=20, resamp=500)
correlog_LM_raw_incr20
plot(correlog_LM_raw_incr20)

# Increment = 10
correlog_LM_raw_incr10 <- correlog(data$latitude, data$longitude, data$lemna_minor, latlon=TRUE, increment=10, resamp=500)
correlog_LM_raw_incr10
plot(correlog_LM_raw_incr10)

# Increment = 5
correlog_LM_raw_incr5 <- correlog(data$latitude, data$longitude, data$lemna_minor, latlon=TRUE, increment=5, resamp=500)
correlog_LM_raw_incr5
plot(correlog_LM_raw_incr5)

# Increment = 2
correlog_LM_raw_incr2 <- correlog(data$latitude, data$longitude, data$lemna_minor, latlon=TRUE, increment=2, resamp=500)
correlog_LM_raw_incr2
plot(correlog_LM_raw_incr2)

#######################
# Moran's I           #
# Spatial correlogram #
# GLM resids          #
#######################
# Increment = 20
correlog_LM_resid_incr20 <- correlog(data$latitude, data$longitude, best_glm_LM_trans_resid, latlon=TRUE, increment=20, resamp=500)
correlog_LM_resid_incr20
plot(correlog_LM_resid_incr20)

# Increment = 10
correlog_LM_resid_incr10 <- correlog(data$latitude, data$longitude, best_glm_LM_trans_resid, latlon=TRUE, increment=10, resamp=500)
correlog_LM_resid_incr10
plot(correlog_LM_resid_incr10)

# Increment = 5
correlog_LM_resid_incr5 <- correlog(data$latitude, data$longitude, best_glm_LM_trans_resid, latlon=TRUE, increment=5, resamp=500)
correlog_LM_resid_incr5
plot(correlog_LM_resid_incr5)

# Increment = 2
correlog_LM_resid_incr2 <- correlog(data$latitude, data$longitude, best_glm_LM_trans_resid, latlon=TRUE, increment=2, resamp=500)
correlog_LM_resid_incr2
plot(correlog_LM_resid_incr2)

#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 10
x <- seq(increment,200,increment)
y <- correlog_LM_resid_incr10$correlation
p <- correlog_LM_resid_incr10$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_LM_raw_incr10$correlation
p <- correlog_LM_raw_incr10$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[1] <- "Significant"
data_temp$signif[21] <- "Significant"
data_temp$signif[28] <- "Significant"
data_temp$signif[30] <- "Significant"
data_temp$signif[33] <- "Significant"
data_temp$signif[34] <- "Significant"
data_temp$signif[38] <- "Significant"

correlogram_LM_incr10 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_LM_incr10 <- correlogram_LM_incr10 + geom_point(aes(shape=signif),size=3)
correlogram_LM_incr10 <- correlogram_LM_incr10 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_LM_incr10 <- correlogram_LM_incr10 + geom_line()
correlogram_LM_incr10 <- correlogram_LM_incr10 + xlab("Distance class (km)")
correlogram_LM_incr10 <- correlogram_LM_incr10 + ylab("Moran's I")
correlogram_LM_incr10 <- correlogram_LM_incr10 + ylim(-0.8,0.8)
correlogram_LM_incr10 <- correlogram_LM_incr10 + geom_hline(yintercept=0,linetype="longdash")
correlogram_LM_incr10 <- correlogram_LM_incr10 + ggtitle("Lemna minor")
correlogram_LM_incr10 <- correlogram_LM_incr10 + scale_linetype(name="Data")
correlogram_LM_incr10 <- correlogram_LM_incr10 + theme_classic(base_size=18)
correlogram_LM_incr10 <- correlogram_LM_incr10 + theme(plot.title=element_text(face="italic")) 
correlogram_LM_incr10 

ggsave("Correlogram - Lemna minor - incr10.jpg",correlogram_LM_incr10,height=8,width=11)


#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 20
x <- seq(increment,200,increment)
y <- correlog_LM_resid_incr20$correlation
p <- correlog_LM_resid_incr20$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_LM_raw_incr20$correlation
p <- correlog_LM_raw_incr20$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[11] <- "Significant"
data_temp$signif[17] <- "Significant"
data_temp$signif[19] <- "Significant"

correlogram_LM_incr20 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_LM_incr20 <- correlogram_LM_incr20 + geom_point(aes(shape=signif),size=3)
correlogram_LM_incr20 <- correlogram_LM_incr20 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_LM_incr20 <- correlogram_LM_incr20 + geom_line()
correlogram_LM_incr20 <- correlogram_LM_incr20 + xlab("Distance class (km)")
correlogram_LM_incr20 <- correlogram_LM_incr20 + ylab("Moran's I")
correlogram_LM_incr20 <- correlogram_LM_incr20 + ylim(-0.8,0.8)
correlogram_LM_incr20 <- correlogram_LM_incr20 + geom_hline(yintercept=0,linetype="longdash")
correlogram_LM_incr20 <- correlogram_LM_incr20 + ggtitle("Lemna minor")
correlogram_LM_incr20 <- correlogram_LM_incr20 + scale_linetype(name="Data")
correlogram_LM_incr20 <- correlogram_LM_incr20 + theme_classic(base_size=18)
correlogram_LM_incr20 <- correlogram_LM_incr20 + theme(plot.title=element_text(face="italic")) 
correlogram_LM_incr20 

ggsave("Correlogram - Lemna minor - incr20.jpg",correlogram_LM_incr20,height=8,width=11)



#############################
# plot raw & resid together #
#############################
# set up the data frame with x (dist class), Y (moran's I) for raw & resid, and p-values 
increment <- 5
x <- seq(increment,200,increment)
y <- correlog_LM_resid_incr5$correlation
p <- correlog_LM_resid_incr5$p
data_temp <- as.data.frame(cbind(x,y,p))
data_temp$data_type <- "Residuals"

y <- correlog_LM_raw_incr5$correlation
p <- correlog_LM_raw_incr5$p
data_temp2 <- as.data.frame(cbind(x,y,p))
data_temp2$data_type <- "Raw"

data_temp <- rbind(data_temp,data_temp2)

# The significance levels is not set @ 0.05 
# Not sure if this is because of a Bonferroni correction ?
data_temp$signif <- "Not significant" # first set them all to significant 
data_temp$signif[11] <- "Significant"
data_temp$signif[17] <- "Significant"
data_temp$signif[19] <- "Significant"

correlogram_LM_incr5 <- ggplot(data_temp,aes(x=x,y=y,group=data_type,linetype=data_type))  
correlogram_LM_incr5 <- correlogram_LM_incr5 + geom_point(aes(shape=signif),size=3)
correlogram_LM_incr5 <- correlogram_LM_incr5 + scale_shape_manual(name="Significance",values=c(1,19))
correlogram_LM_incr5 <- correlogram_LM_incr5 + geom_line()
correlogram_LM_incr5 <- correlogram_LM_incr5 + xlab("Distance class (km)")
correlogram_LM_incr5 <- correlogram_LM_incr5 + ylab("Moran's I")
correlogram_LM_incr5 <- correlogram_LM_incr5 + ylim(-0.8,0.8)
correlogram_LM_incr5 <- correlogram_LM_incr5 + geom_hline(yintercept=0,linetype="longdash")
correlogram_LM_incr5 <- correlogram_LM_incr5 + ggtitle("Lemna minor")
correlogram_LM_incr5 <- correlogram_LM_incr5 + scale_linetype(name="Data")
correlogram_LM_incr5 <- correlogram_LM_incr5 + theme_classic(base_size=18)
correlogram_LM_incr5 <- correlogram_LM_incr5 + theme(plot.title=element_text(face="italic")) 
correlogram_LM_incr5 

ggsave("Correlogram - Lemna minor - incr5.jpg",correlogram_LM_incr5,height=8,width=11)



################################
# Cobmine all species together #
################################
library(gridExtra)
all_sp_correlograms <- arrangeGrob(correlogram_LM_incr10,correlogram_SP_incr10,correlogram_W_incr10,
             correlogram_LM_incr20,correlogram_LM_incr20,correlogram_W_incr20, 
             ncol=3)
ggsave("all_sp_correlograms.jpg",all_sp_correlograms,height=8,width=11)

