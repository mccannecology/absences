library(ggplot2)
library(fields)
library(gdata)
head(data)

##################################################
# Get dataframe of lat/lon for lakes & each spp. #
################################################## 
coords_ALL <- as.data.frame(cbind(data$longitude,data$latitude))
colnames(coords_ALL) <- c("lon","lat")

coords_LM <- as.data.frame(cbind(subset(data$longitude,data$lemna_minor==1),
                                 subset(data$latitude,data$lemna_minor==1)))
colnames(coords_LM) <- c("lon","lat")

coords_SP <- as.data.frame(cbind(subset(data$longitude,data$spirodela_polyrhiza==1),
                                 subset(data$latitude,data$spirodela_polyrhiza==1)))
colnames(coords_SP) <- c("lon","lat")

coords_W <- as.data.frame(cbind(subset(data$longitude,data$wolffia_sp==1),
                                 subset(data$latitude,data$wolffia_sp==1)))
colnames(coords_W) <- c("lon","lat")

#############################
# Calculate distance matrix #
#############################
library(fields)
dist_ALL <- rdist.earth(coords_ALL,miles=FALSE)
dist_LM <- rdist.earth(coords_LM,miles=FALSE)
dist_SP <- rdist.earth(coords_SP,miles=FALSE)
dist_W <- rdist.earth(coords_W,miles=FALSE)

#################################
# Get a vector of the distances #
#################################
library(gdata)
dist_ALL <- upperTriangle(dist_ALL, diag=FALSE)
dist_LM <- upperTriangle(dist_LM, diag=FALSE)
dist_SP <- upperTriangle(dist_SP, diag=FALSE)
dist_W <- upperTriangle(dist_W, diag=FALSE)

############################################ 
# remove dists>100 a la Capers et al. 2009 #
############################################ 
dist_ALL <- subset(dist_ALL, dist_ALL<=100)
dist_LM <- subset(dist_LM, dist_LM<=100)
dist_SP <- subset(dist_SP, dist_SP<=100)
dist_W <- subset(dist_W, dist_W<=100)

#########################
# Put dists. in classes #
#########################
bins_ALL <- table(cut(dist_ALL,breaks=c(0,20,40,60,80,100)))
bins_LM <- table(cut(dist_LM,breaks=c(0,20,40,60,80,100)))
bins_SP <- table(cut(dist_SP,breaks=c(0,20,40,60,80,100)))
bins_W <- table(cut(dist_W,breaks=c(0,20,40,60,80,100)))

#########################
# build table w/ freqs. #
######################### 
dist_table_ALL <- as.data.frame(cbind(c(10,30,50,70,90), c(bins_ALL[1],bins_ALL[2],bins_ALL[3],bins_ALL[4],bins_ALL[5])))
colnames(dist_table_ALL) <- c("dist_mid","freq")

dist_table_LM <- as.data.frame(cbind(c(10,30,50,70,90), c(bins_LM[1],bins_LM[2],bins_LM[3],bins_LM[4],bins_LM[5])))
colnames(dist_table_LM) <- c("dist_mid","freq")

dist_table_SP <- as.data.frame(cbind(c(10,30,50,70,90), c(bins_SP[1],bins_SP[2],bins_SP[3],bins_SP[4],bins_SP[5])))
colnames(dist_table_SP) <- c("dist_mid","freq")

dist_table_W <- as.data.frame(cbind(c(10,30,50,70,90), c(bins_W[1],bins_W[2],bins_W[3],bins_W[4],bins_W[5])))
colnames(dist_table_W) <- c("dist_mid","freq")

#####################
# calc. rel. freqs. #
#####################
dist_table_ALL$rel_freq <- dist_table_ALL$freq / sum(dist_table_ALL$freq)
dist_table_LM$rel_freq <- dist_table_LM$freq / sum(dist_table_LM$freq)
dist_table_SP$rel_freq <- dist_table_SP$freq / sum(dist_table_SP$freq)
dist_table_W$rel_freq <- dist_table_W$freq / sum(dist_table_W$freq)

###############################################
# re-arrange data.frames into one for ggplot2 #
###############################################
dist_table_ALL$class <- "All lakes"
dist_table_LM$class <- "Lemna minor"
dist_table_SP$class <- "Spirodela polyrhiza"
dist_table_W$class <- "Wolffia sp."

dist_table_plotting <- rbind(dist_table_ALL,dist_table_LM,dist_table_SP,dist_table_W)

###########
# plot it #
###########
inter_dist_plot <- ggplot(dist_table_plotting,aes(x=dist_mid,y=rel_freq,group=class,shape=class)) + geom_point(size=3) + geom_line(aes(linetype=class))
inter_dist_plot <- inter_dist_plot + ylab("Relative frequency")
inter_dist_plot <- inter_dist_plot + xlab("Distance class (km)")
inter_dist_plot <- inter_dist_plot + ylim(c(0,0.3)) 
inter_dist_plot <- inter_dist_plot + theme_classic(base_size=18) 
inter_dist_plot <- inter_dist_plot + theme(legend.title=element_blank())
inter_dist_plot

ggsave("inter_dist_plot.jpg",inter_dist_plot,height=8,width=11)

###################
# Signif. testing #
###################
occurences <- nrow(coords_SP) # how many lakes have the species of interest 
numb_iterations <- 1000

# create a vector to hold the results for each size class 
distance_10 <- NULL 
distance_30 <- NULL 
distance_50 <- NULL 
distance_70 <- NULL 
distance_90 <- NULL 

# this will get iterated X number of times
for (i in 1:numb_iterations){
  coords_RAND <- coords_ALL[sample(nrow(coords_ALL),occurences),] # randomly sample from all lakes 
  dist_RAND <- rdist.earth(coords_RAND,miles=FALSE) # Calculate distance matrix 
  dist_RAND <- upperTriangle(dist_RAND, diag=FALSE) # Get a vector of the distances
  dist_RAND <- subset(dist_RAND, dist_RAND<=100) # remove dists>100 a la Capers et al. 2009
  bins_RAND <- table(cut(dist_RAND,breaks=c(0,20,40,60,80,100))) # Put dists. in classes
  dist_table_RAND <- as.data.frame(cbind(c(10,30,50,70,90), c(bins_RAND[1],bins_RAND[2],bins_RAND[3],bins_RAND[4],bins_RAND[5]))) # build table w/ freqs.
  colnames(dist_table_RAND) <- c("dist_mid","freq")
  dist_table_RAND$rel_freq <- dist_table_RAND$freq / sum(dist_table_RAND$freq) # re-arrange data.frames into one for ggplot2
  
  distance_10 <- c(distance_10,dist_table_RAND$rel_freq[1])
  distance_30 <- c(distance_30,dist_table_RAND$rel_freq[2])
  distance_50 <- c(distance_50,dist_table_RAND$rel_freq[3])
  distance_70 <- c(distance_70,dist_table_RAND$rel_freq[4])
  distance_90 <- c(distance_90,dist_table_RAND$rel_freq[5])
}

distance_10_SP <- distance_10 
distance_30_SP <- distance_30
distance_50_SP <- distance_50
distance_70_SP <- distance_70
distance_90_SP <- distance_90

###################################
# Plot histogram & observed value # 
###################################
jpeg("distance_LM.jpg",height=4,width=12,units="in",res=300)
par(mfrow=c(1,5))
hist(distance_10_LM) + abline(v=dist_table_LM$rel_freq[1],col="red",lty=2)
hist(distance_30_LM) + abline(v=dist_table_LM$rel_freq[2],col="red",lty=2)
hist(distance_50_LM) + abline(v=dist_table_LM$rel_freq[3],col="red",lty=2)
hist(distance_70_LM) + abline(v=dist_table_LM$rel_freq[4],col="red",lty=2)
hist(distance_90_LM) + abline(v=dist_table_LM$rel_freq[5],col="red",lty=2)
dev.off()

jpeg("distance_SP.jpg",height=4,width=12,units="in",res=300)
par(mfrow=c(1,5))
hist(distance_10_SP) + abline(v=dist_table_SP$rel_freq[1],col="red",lty=2)
hist(distance_30_SP) + abline(v=dist_table_SP$rel_freq[2],col="red",lty=2)
hist(distance_50_SP) + abline(v=dist_table_SP$rel_freq[3],col="red",lty=2)
hist(distance_70_SP) + abline(v=dist_table_SP$rel_freq[4],col="red",lty=2)
hist(distance_90_SP) + abline(v=dist_table_SP$rel_freq[5],col="red",lty=2)
dev.off()

jpeg("distance_W.jpg",height=4,width=12,units="in",res=300)
par(mfrow=c(1,5))
hist(distance_10_W) + abline(v=dist_table_W$rel_freq[1],col="red",lty=2)
hist(distance_30_W) + abline(v=dist_table_W$rel_freq[2],col="red",lty=2)
hist(distance_50_W) + abline(v=dist_table_W$rel_freq[3],col="red",lty=2)
hist(distance_70_W) + abline(v=dist_table_W$rel_freq[4],col="red",lty=2)
hist(distance_90_W) + abline(v=dist_table_W$rel_freq[5],col="red",lty=2)
dev.off()

############################ 
# Statistical significance # 
############################
1-length(subset(distance_10_LM, distance_10_LM<dist_table_LM$rel_freq[1]))/1000
1-length(subset(distance_30_LM, distance_30_LM<dist_table_LM$rel_freq[2]))/1000
1-length(subset(distance_50_LM, distance_50_LM>dist_table_LM$rel_freq[3]))/1000
1-length(subset(distance_70_LM, distance_70_LM>dist_table_LM$rel_freq[4]))/1000
1-length(subset(distance_90_LM, distance_90_LM>dist_table_LM$rel_freq[5]))/1000

1-length(subset(distance_10_SP, distance_10_SP<dist_table_SP$rel_freq[1]))/1000
1-length(subset(distance_30_SP, distance_30_SP>dist_table_SP$rel_freq[2]))/1000
1-length(subset(distance_50_SP, distance_50_SP<dist_table_SP$rel_freq[3]))/1000
1-length(subset(distance_70_SP, distance_70_SP<dist_table_SP$rel_freq[4]))/1000
1-length(subset(distance_90_SP, distance_90_SP<dist_table_SP$rel_freq[5]))/1000

1-length(subset(distance_10_SP, distance_10_SP<dist_table_SP$rel_freq[1]))/1000
1-length(subset(distance_30_SP, distance_30_SP<dist_table_SP$rel_freq[2]))/1000
1-length(subset(distance_50_SP, distance_50_SP<dist_table_SP$rel_freq[3]))/1000
1-length(subset(distance_70_SP, distance_70_SP>dist_table_SP$rel_freq[4]))/1000
1-length(subset(distance_90_SP, distance_90_SP<dist_table_SP$rel_freq[5]))/1000
