library(vegan)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

###############
# All species #
# dataSPECIES #
###############

# convert dataSPECIES to a distance matrix 
dist_SPECIES <- dist(dataSPECIES)

# NMDS
library(vegan)
set.seed(2)
species_mds <- metaMDS(dataSPECIES, k=2) 
species_mds
# stress plot 
stressplot(species_mds)
# plot it 
plot(species_mds)
# label species 
orditorp(species_mds,display="species",col="red",air=0.01)

# Fitting environmental 
species_envfit <- envfit(species_mds, dataENV, permu=999)

# plot both 
plot(species_mds)
plot(species_envfit)

###############
# FP species  #
# dataFP      #
###############
# convert dataFP to a distance matrix 
dist_FP <- dist(dataFP)

# NMDS
library(vegan)
set.seed(2)
FP_mds <- metaMDS(dataFP, k=2, distance="euclidean") 
FP_mds
# stress plot 
stressplot(FP_mds)
# plot it 
plot(FP_mds)
# label species 
orditorp(FP_mds,display="species",col="red",air=0.01)
