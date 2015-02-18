################################ 
# Pricipal Components Analysis #
################################
# http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Principal-Component-Analysis-PCA-

colnames(dataENV)

# get rid of variables that you don't need 
dataPCA <- dataENV

colnames(dataPCA)

#######################################
# Perform PCA after scaling the data. #
#######################################
# Returns a list with class "prcomp" w/ five components: 
# (1) standard deviations (sdev) of the principal components, 
# (2) the matrix of eigenvectors (rotation), 
# (3) the principal component data (x), 
# (4) the centering (center) and 
# (5) scaling (scale) used.

pca <- prcomp(dataPCA, scale=T) 

# Prints variance summary for all principal components.
summary(pca)

# eigenvectors (loadings of variables on each PC axis)
pca[2]

# scree plot (amount of variation per PC axis)
plot(pca)

# scatterplot of points 
plot(pca$x)

# scatterplot of points w/ vectors of variables
biplot(pca)

jpeg(file="PCA - dataENV.jpg",height=8,width=8,units="in",res=300)
biplot(pca)
dev.off()


#############################
# PCA - with center & scale #
#############################
pca_01 <- prcomp(dataPCA, center=T, scale=T) 
summary(pca_01)
pca_01[2]
plot(pca_01)
biplot(pca_01)

#####################################
# PCA - with log(x+1) normalization #
#####################################
pca_02 <- prcomp(log(dataPCA+1), center=F, scale=F) 
summary(pca_02)
pca_02[2]
plot(pca_02)
biplot(pca_02)

#####################################
# PCA - with log(x+1) normalization #
# center & scale                    #
#####################################
pca_03 <- prcomp(log(dataPCA+1), center=T, scale=T) 
summary(pca_03)
pca_03[2]
plot(pca_03)
biplot(pca_03)

#####################################
# PCA - with log(x+1) normalization #
# center                            #
#####################################
pca_04 <- prcomp(log(dataPCA+1), center=T, scale=F) 
summary(pca_04)
pca_04[2]
plot(pca_04)
biplot(pca_04)

#####################################
# PCA - with log(x+1) normalization #
# scale                             #
#####################################
pca_05 <- prcomp(log(dataPCA+1), center=F,scale=T) 
summary(pca_05)
pca_05[2]
plot(pca_05)
biplot(pca_05)



################################################
# PCA - with log(x+1) normalization & centered # This looks like it is the same as not specifiying centering 
################################################ 
pca_logx1_center <- prcomp(log(dataPCA+1), scale=T, center=T) 
summary(pca_logx1_center)
pca_logx1_center[2]
plot(pca_logx1_center)
biplot(pca_logx1_center)

###############################
# Alternative way to do a PCA #
###############################
pca <- princomp(dataPCA, scores=T) 
summary(pca)
print(pca)
pca$scores[,1] # scores for the first principal component
biplot(pca) # biplot 
plot(pca) # scree plot

###############################
# Re-do and include lat & lon #
###############################
dataPCA2 <- cbind(dataPCA, dataSPACE$latitude, dataSPACE$longitude)
colnames(dataPCA2)
colnames(dataPCA2)[11] <- "latitude"
colnames(dataPCA2)[12] <- "longitude"
colnames(dataPCA2)

pca2 <- prcomp(dataPCA2, scale=T)

summary(pca2)

pca2[2]

biplot(pca2)
