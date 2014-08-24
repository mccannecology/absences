################################ 
# Pricipal Components Analysis #
################################
# http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Principal-Component-Analysis-PCA-

colnames(dataENV)

dataPCA <- dataENV
dataPCA$waterbodies_1km <- NULL 
dataPCA$waterbodies_5km <- NULL 
dataPCA$waterbodies_10km <- NULL 
dataPCA$nearest_LM <- NULL 
dataPCA$nearest_SP <- NULL 
dataPCA$nearest_W <- NULL 
dataPCA$nearest_LMSPW <- NULL 

colnames(dataPCA)

# Perform PCA after scaling the data. 
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
