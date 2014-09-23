library(vegan)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

########################
# Ordination of        #  
# site similarity      #
# subset of dataENV    #
# local variables only # 
########################
dataENV_local <- dataENV[,1:8]

# convert to a distance matrix 
dist_ENV_local <- dist(dataENV_local)

library(vegan)
set.seed(2)
ENV_local_mds <- metaMDS(dist_ENV_local, k=2) 
ENV_local_mds

stressplot(ENV_local_mds)

plot(ENV_local_mds)

colvec <- c("black","green")

# Custom ordination plots 
# using base plotting and vegan objects 
# http://www.fromthebottomoftheheap.net/2012/04/11/customising-vegans-ordination-plots/

# lemna minor 
jpeg("NMDS - local vars only - lemna_minor.jpg",units="in",height=8,width=8,res=300)
plot(ENV_local_mds,main="L.minor - local vars. only")
with(dataFP, points(ENV_local_mds, display="sites",col=colvec[as.factor(lemna_minor)],pch=21,bg=colvec[as.factor(lemna_minor)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0465")
dev.off()

# spirodela polyrhiza
jpeg("NMDS - local vars only - spirodela_polyrhiza.jpg",units="in",height=8,width=8,res=300)
plot(ENV_local_mds,main="S.polyrhiza - local vars. only")
with(dataFP, points(ENV_local_mds, display="sites",col=colvec[as.factor(spirodela_polyrhiza)],pch=21,bg=colvec[as.factor(spirodela_polyrhiza)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0465")
dev.off()

# wolffia sp 
jpeg("NMDS - local vars only - wolffia.jpg",units="in",height=8,width=8,res=300)
plot(ENV_local_mds,main="Wolffia sp. - local vars. only")
with(dataFP, points(ENV_local_mds, display="sites",col=colvec[as.factor(wolffia)],pch=21,bg=colvec[as.factor(wolffia)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0465")
dev.off()

# FP presence 
jpeg("NMDS - local vars only - FPpresence.jpg",units="in",height=8,width=8,res=300)
plot(ENV_local_mds,main="FP presence - local vars. only")
with(data, points(ENV_local_mds, display="sites",col=colvec[FP_presence],pch=21,bg=colvec[FP_presence]))
with(data, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0465")
dev.off()


########################
# Ordination of        #  
# site similarity      #
# all X variables      # 
########################
# convert to a distance matrix 
dist_ENV <- dist(dataENV)

library(vegan)
set.seed(2)
ENV_mds <- metaMDS(dist_ENV, k=2) 
ENV_mds

stressplot(ENV_mds)

plot(ENV_mds)

colvec <- c("black","green")

# Custom ordination plots 
# using base plotting and vegan objects 
# http://www.fromthebottomoftheheap.net/2012/04/11/customising-vegans-ordination-plots/

# lemna minor 
jpeg("NMDS - all vars - lemna_minor.jpg",units="in",height=8,width=8,res=300)
plot(ENV_mds,main="L.minor - all vars")
with(dataFP, points(ENV_mds, display="sites",col=colvec[as.factor(lemna_minor)],pch=21,bg=colvec[as.factor(lemna_minor)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0621")
dev.off()

# spirodela polyrhiza
jpeg("NMDS - all vars - spirodela_polyrhiza.jpg",units="in",height=8,width=8,res=300)
plot(ENV_mds,main="S.polyrhiza - all vars")
with(dataFP, points(ENV_mds, display="sites",col=colvec[as.factor(spirodela_polyrhiza)],pch=21,bg=colvec[as.factor(spirodela_polyrhiza)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0621")
dev.off()

# wolffia sp 
jpeg("NMDS - all vars - wolffia.jpg",units="in",height=8,width=8,res=300)
plot(ENV_mds,main="Wolffia sp. - all vars")
with(dataFP, points(ENV_mds, display="sites",col=colvec[as.factor(wolffia)],pch=21,bg=colvec[as.factor(wolffia)]))
with(dataFP, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0621")
dev.off()

# FP presence 
jpeg("NMDS - all vars - FPpresence.jpg",units="in",height=8,width=8,res=300)
plot(ENV_mds,main="FP presence - all vars")
with(data, points(ENV_mds, display="sites",col=colvec[FP_presence],pch=21,bg=colvec[FP_presence]))
with(data, legend("bottomright",legend=c("Absent","Present"),bty="n",col=colvec,pch=21,pt.bg=colvec))
text(x=275,y=100,labels="Stress = 0.0621")
dev.off()



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
