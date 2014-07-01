library(vegan)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

# convert dataSPECIES 
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
