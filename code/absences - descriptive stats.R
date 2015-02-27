library(ggplot2)
library(reshape2)
library(plyr)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES
dataSPECIES_freq

################
# Scatterplots #
# FP presence  #
################
jpeg("predictors vs FP presence.jpg",height=8,width=11,units="in",res=300)
par(mfrow=c(4,4))
plot(data$TOTP_avg,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Total P(mg/L)")
plot(data$surfacearea_ha,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Surface area (ha)")
plot(data$depth_max_m,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Max depth (m)")
plot(data$nonFP_presence,data$FP_presence,ylab="Floating plant presence",xlab="Non-FP species richness")
plot(data$PH_avg,data$FP_presence,log="x",ylab="Floating plant presence",xlab="pH")
plot(data$COND_avg,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Conductivity (uS)")
plot(data$ALK_avg,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Alkalinity (mg/L)")
plot(data$secchi_avg,data$FP_presence,log="x",ylab="Floating plant presence",xlab="Secchi depth (m)")
plot(data$waterbodies_1km,data$FP_presence,ylab="Floating plant presence",xlab="# waterbodies <1 km")
plot(data$waterbodies_5km,data$FP_presence,log="x",ylab="Floating plant presence",xlab="waterbodies <5 km")
plot(data$waterbodies_10km,data$FP_presence,log="x",ylab="Floating plant presence",xlab="# waterbodies <10 km")
plot(data$boatlaunch,data$FP_presence,ylab="Floating plant presence",xlab="Boat launch presence")
plot(data$dist_waterfowl,data$FP_presence,ylab="Floating plant presence",xlab="Distance to migratory waterfowl habitat (m)")
plot(data$nearest_LM,data$FP_presence,ylab="Floating plant presence",xlab="Distance to nearest L. minor (m)")
plot(data$nearest_SP,data$FP_presence,ylab="Floating plant presence",xlab="Distance to nearest S. polyrhiza (m)")
plot(data$nearest_W,data$FP_presence,ylab="Floating plant presence",xlab="Distance to nearest Wolffia sp. (m)")
dev.off()


################
# Scatterplots #
# FP richness  #
################
jpeg("predictors vs FP richness.jpg",height=8,width=11,units="in",res=300)
par(mfrow=c(4,4))
plot(data$TOTP_avg,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Total P(mg/L)")
plot(data$surfacearea_ha,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Surface area (ha)")
plot(data$depth_max_m,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Max depth (m)")
plot(data$nonFP_species_richness,data$FP_species_richness,ylab="Floating plant species richness",xlab="Non-FP species richness")
plot(data$PH_avg,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="pH")
plot(data$COND_avg,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Conductivity (uS)")
plot(data$ALK_avg,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Alkalinity (mg/L)")
plot(data$secchi_avg,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="Secchi depth (m)")
plot(data$waterbodies_1km,data$FP_species_richness,ylab="Floating plant species richness",xlab="# waterbodies <1 km")
plot(data$waterbodies_5km,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="waterbodies <5 km")
plot(data$waterbodies_10km,data$FP_species_richness,log="x",ylab="Floating plant species richness",xlab="# waterbodies <10 km")
plot(data$boatlaunch,data$FP_species_richness,ylab="Floating plant species richness",xlab="Boat launch presence")
plot(data$dist_waterfowl,data$FP_species_richness,ylab="Floating plant species richness",xlab="Distance to migratory waterfowl habitat (m)")
plot(data$nearest_LM,data$FP_species_richness,ylab="Floating plant species richness",xlab="Distance to nearest L. minor (m)")
plot(data$nearest_SP,data$FP_species_richness,ylab="Floating plant species richness",xlab="Distance to nearest S. polyrhiza (m)")
plot(data$nearest_W,data$FP_species_richness,ylab="Floating plant species richness",xlab="Distance to nearest Wolffia sp. (m)")
dev.off()

################
# Scatterplots #
# lemna_minor  #
################
jpeg("predictors vs lemna_minor.jpg",height=8,width=11,units="in",res=300)
par(mfrow=c(4,4))
plot(data$TOTP_avg,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Total P(mg/L)")
plot(data$surfacearea_ha,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Surface area (ha)")
plot(data$depth_max_m,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Max depth (m)")
plot(data$nonFP_species_richness,data$lemna_minor,ylab="Lemna minor",xlab="Non-FP species richness")
plot(data$PH_avg,data$lemna_minor,log="x",ylab="Lemna minor",xlab="pH")
plot(data$COND_avg,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Conductivity (uS)")
plot(data$ALK_avg,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Alkalinity (mg/L)")
plot(data$secchi_avg,data$lemna_minor,log="x",ylab="Lemna minor",xlab="Secchi depth (m)")
plot(data$waterbodies_1km,data$lemna_minor,ylab="Lemna minor",xlab="# waterbodies <1 km")
plot(data$waterbodies_5km,data$lemna_minor,log="x",ylab="Lemna minor",xlab="waterbodies <5 km")
plot(data$waterbodies_10km,data$lemna_minor,log="x",ylab="Lemna minor",xlab="# waterbodies <10 km")
plot(data$boatlaunch,data$lemna_minor,ylab="Lemna minor",xlab="Boat launch presence")
plot(data$dist_waterfowl,data$lemna_minor,ylab="Lemna minor",xlab="Distance to migratory waterfowl habitat (m)")
plot(data$nearest_LM,data$lemna_minor,ylab="Lemna minor",xlab="Distance to nearest L. minor (m)")
plot(data$nearest_SP,data$lemna_minor,ylab="Lemna minor",xlab="Distance to nearest S. polyrhiza (m)")
plot(data$nearest_W,data$lemna_minor,ylab="Lemna minor",xlab="Distance to nearest Wolffia sp. (m)")
dev.off()

################
# Scatterplots #
# spirodela    #
################
jpeg("predictors vs spirodela_polyrhiza.jpg",height=8,width=11,units="in",res=300)
par(mfrow=c(4,4))
plot(data$TOTP_avg,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Total P(mg/L)")
plot(data$surfacearea_ha,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Surface area (ha)")
plot(data$depth_max_m,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Max depth (m)")
plot(data$nonFP_species_richness,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Non-FP species richness")
plot(data$PH_avg,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="pH")
plot(data$COND_avg,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Conductivity (uS)")
plot(data$ALK_avg,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Alkalinity (mg/L)")
plot(data$secchi_avg,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="Secchi depth (m)")
plot(data$waterbodies_1km,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="# waterbodies <1 km")
plot(data$waterbodies_5km,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="waterbodies <5 km")
plot(data$waterbodies_10km,data$spirodela_polyrhiza,log="x",ylab="Spirodela polyrhiza",xlab="# waterbodies <10 km")
plot(data$boatlaunch,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Boat launch presence")
plot(data$dist_waterfowl,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Distance to migratory waterfowl habitat (m)")
plot(data$nearest_LM,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Distance to nearest L. minor (m)")
plot(data$nearest_SP,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Distance to nearest S. polyrhiza (m)")
plot(data$nearest_W,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Distance to nearest Wolffia sp. (m)")
dev.off()

################
# Scatterplots #
# wolffia_sp   #
################
# data$wolffia_sp <- data$wolffia+ data$wolffia_brasiliensis + data$wolffia_borealis # I already did this in "absences - import.R" 

jpeg("predictors vs wolffia_sp.jpg",height=8,width=11,units="in",res=300)
par(mfrow=c(4,4))
plot(data$TOTP_avg,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Total P(mg/L)")
plot(data$surfacearea_ha,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Surface area (ha)")
plot(data$depth_max_m,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Max depth (m)")
plot(data$nonFP_species_richness,data$wolffia_sp,ylab="Wolffia sp.",xlab="Non-FP species richness")
plot(data$PH_avg,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="pH")
plot(data$COND_avg,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Conductivity (uS)")
plot(data$ALK_avg,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Alkalinity (mg/L)")
plot(data$secchi_avg,data$wolffia_sp,log="x",ylab="Wolffia sp.",xlab="Secchi depth (m)")
plot(data$waterbodies_1km,data$wolffia_sp,ylab="Wolffia sp",xlab="# waterbodies <1 km")
plot(data$waterbodies_5km,data$wolffia_sp,log="x",ylab="Wolffia sp",xlab="waterbodies <5 km")
plot(data$waterbodies_10km,data$wolffia_sp,log="x",ylab="Wolffia sp",xlab="# waterbodies <10 km")
plot(data$boatlaunch,data$wolffia_sp,ylab="Wolffia sp",xlab="Boat launch presence")
plot(data$dist_waterfowl,data$wolffia_sp,ylab="Wolffia sp",xlab="Distance to migratory waterfowl habitat (m)")
plot(data$nearest_LM,data$wolffia_sp,ylab="Wolffia sp",xlab="Distance to nearest L. minor (m)")
plot(data$nearest_SP,data$wolffia_sp,ylab="Wolffia sp",xlab="Distance to nearest S. polyrhiza (m)")
plot(data$nearest_W,data$wolffia_sp,ylab="Wolffia sp",xlab="Distance to nearest Wolffia sp. (m)")
dev.off()

#############################
# Histogram of sp. richness #
# Floating plants           #
#############################
FP_richness_vector <- as.data.frame(rowSums(dataFP))
colnames(FP_richness_vector)[1]<-"richness"
FP_richness_vector
table(FP_richness_vector)
FP_richness_matrix <- matrix(c(0,1,2,3,4,104,39,21,11,1),nrow=5,ncol=2,byrow=F)
FP_richness_matrix <- as.data.frame(FP_richness_matrix)
colnames(FP_richness_matrix)[1] <- "richness"
colnames(FP_richness_matrix)[2] <- "frequency"
FP_richness_matrix

hist_FP_rich <- ggplot(FP_richness_matrix, aes(x=richness,y=frequency)) + geom_bar(stat="identity",fill = I("grey50"))
hist_FP_rich <- hist_FP_rich + xlab("Floating plant richness")
hist_FP_rich <- hist_FP_rich + ylab("Frequency")
hist_FP_rich <- hist_FP_rich + theme_classic(base_size=18)
hist_FP_rich
ggsave("hist_FP_rich.jpg",hist_FP_rich,height=8,width=8)

#############################
# Histogram of sp. richness #
# Non-floating plants       #
#############################
NONFP_richness_vector <- as.data.frame(rowSums(dataNONFP))
colnames(NONFP_richness_vector)[1]<-"richness"
NONFP_richness_vector
table(NONFP_richness_vector)
NONFP_richness_matrix <- as.data.frame.table(table(NONFP_richness_vector))
colnames(NONFP_richness_matrix)[1] <- "richness"
colnames(NONFP_richness_matrix)[2] <- "frequency"
NONFP_richness_matrix

hist_NONFP_rich <- ggplot(NONFP_richness_matrix, aes(x=richness,y=frequency)) + geom_bar(stat="identity",fill = I("grey50"))
hist_NONFP_rich <- hist_NONFP_rich + xlab("Non-floating plant richness")
hist_NONFP_rich <- hist_NONFP_rich + ylab("Frequency")
hist_NONFP_rich <- hist_NONFP_rich + theme_classic(base_size=18)
hist_NONFP_rich
ggsave("hist_NONFP_rich.jpg",hist_NONFP_rich,height=8,width=11)

#################################
# Histogram of # of waterbodies #
# each species is found in      #
# with functional groups labels #
#################################
# if you want this plot with species listed alphabetically,
# re-do dataSPECIES_freq in "absences - import.R" 
# and leave out ordering the species as a factor of frequency 

dataSPECIES_freq3 <- read.csv("dataSPECIES_freq3_with_func_group_labels.csv")
head(dataSPECIES_freq3)

##########
# colour #
##########
sp_freq_plot_CT <- ggplot(dataSPECIES_freq3,aes(x=reorder(species, -frequency),y=frequency,fill=group))
sp_freq_plot_CT <- sp_freq_plot_CT + geom_bar(aes(fill=group),stat="identity")
sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("green2","violetred2","yellow2","turquoise2"))
sp_freq_plot_CT <- sp_freq_plot_CT + xlab("Species")
sp_freq_plot_CT <- sp_freq_plot_CT + ylab("# of waterbodies found in")
sp_freq_plot_CT <- sp_freq_plot_CT + geom_text(aes(y=frequency+1,label=label))
sp_freq_plot_CT <- sp_freq_plot_CT + ggtitle("Connecticut, USA (n=174)")
sp_freq_plot_CT <- sp_freq_plot_CT + theme_classic()
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + scale_x_discrete(labels=seq(1,nrow(dataSPECIES_freq3),1)) # replace species names with #s
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.x = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.y = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(plot.title = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.y = element_text(size=14))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(size=10))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 0, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.title = element_text(size=16, face="bold"))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.text = element_text(size = 12))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.position=c(.95, .25))
sp_freq_plot_CT

ggsave("sp_freq_plot_CT - with_func_group_labels.jpg",sp_freq_plot_CT,height=8,width=11)


#######
# B&W #
#######
# re-order functional groups so it goes from light to dark on the legend 
dataSPECIES_freq3$group <- factor(dataSPECIES_freq3$group, levels=c("floating", "submerged", "emergent", "lily"))


sp_freq_plot_CT <- ggplot(dataSPECIES_freq3,aes(x=reorder(species, -frequency),y=frequency,fill=group))
sp_freq_plot_CT <- sp_freq_plot_CT + geom_bar(aes(fill=group),stat="identity",colour="black")
################################################################ floating, submerged, emergent, lily
sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("grey90","grey55","grey25","black"))
################################################################ emergent, floating, lily, submerged
#sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("grey28","grey90","black","grey50"))
sp_freq_plot_CT <- sp_freq_plot_CT + xlab("Species")
sp_freq_plot_CT <- sp_freq_plot_CT + ylab("# of waterbodies found in")
sp_freq_plot_CT <- sp_freq_plot_CT + geom_text(aes(y=frequency+1,label=label))
#sp_freq_plot_CT <- sp_freq_plot_CT + ggtitle("Connecticut, USA (n=174)")
sp_freq_plot_CT <- sp_freq_plot_CT + theme_classic()
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + scale_x_discrete(labels=seq(1,nrow(dataSPECIES_freq3),1)) # replace species names with #s
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.x = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.y = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(plot.title = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.y = element_text(size=14))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(size=10))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 270, hjust = 0))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.title = element_text(size=16, face="bold"))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.text = element_text(size = 12))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.position=c(.95, .25))
sp_freq_plot_CT

ggsave("sp_freq_plot_CT - with_func_group_labels - B&W.jpg",sp_freq_plot_CT,height=8,width=11)

<<<<<<< HEAD

sp_freq_plot_CT <- ggplot(dataSPECIES_freq3,aes(x=reorder(species, -frequency),y=frequency,fill=group))
sp_freq_plot_CT <- sp_freq_plot_CT + geom_bar(aes(fill=group),stat="identity",colour="black")
################################################################ floating, submerged, emergent, lily
sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"))
sp_freq_plot_CT <- sp_freq_plot_CT + xlab("Species")
sp_freq_plot_CT <- sp_freq_plot_CT + ylab("# of waterbodies found in")
sp_freq_plot_CT <- sp_freq_plot_CT + geom_text(aes(y=frequency+2,label=name,fontface="italic"),angle=45,hjust=0)
sp_freq_plot_CT <- sp_freq_plot_CT + theme_classic()
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + scale_x_discrete(labels=NULL) # replace species names with #s
sp_freq_plot_CT <- sp_freq_plot_CT + scale_y_continuous(expand=c(0,0))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.x = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.y = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(plot.title = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.y = element_text(size=14))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(size=8))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 270, hjust = 0))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.title = element_text(size=16, face="bold"))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.text = element_text(size = 12))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.position=c(0.85, 0.75))
sp_freq_plot_CT

ggsave("sp_freq_plot_CT - with_func_group_labels - colour_v2.jpg",sp_freq_plot_CT,height=6,width=12)


#################################
# Histogram of # of waterbodies #
# each species is found in      #
# with functional groups labels #
#################################
# if you want this plot with species listed alphabetically,
# re-do dataSPECIES_freq in "absences - import.R" 
# and leave out ordering the species as a factor of frequency 

dataSPECIES_freq3 <- read.csv("dataSPECIES_freq3_with_func_group_labels.csv")
head(dataSPECIES_freq3)

##########
# colour #
##########
sp_freq_plot_CT <- ggplot(dataSPECIES_freq3,aes(x=reorder(species, -frequency),y=frequency,fill=group))
sp_freq_plot_CT <- sp_freq_plot_CT + geom_bar(aes(fill=group),stat="identity")
sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("green2","violetred2","yellow2","turquoise2"))
sp_freq_plot_CT <- sp_freq_plot_CT + xlab("Species")
sp_freq_plot_CT <- sp_freq_plot_CT + ylab("# of waterbodies found in")
sp_freq_plot_CT <- sp_freq_plot_CT + geom_text(aes(y=frequency+1,label=Label))
sp_freq_plot_CT <- sp_freq_plot_CT + ggtitle("Connecticut, USA (n=174)")
sp_freq_plot_CT <- sp_freq_plot_CT + theme_classic()
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + scale_x_discrete(labels=seq(1,nrow(dataSPECIES_freq2),1)) # replace species names with #s
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.x = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.y = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(plot.title = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.y = element_text(size=14))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(size=10))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 0, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.title = element_text(size=16, face="bold"))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.text = element_text(size = 12))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.position=c(.95, .25))
sp_freq_plot_CT

ggsave("sp_freq_plot_CT - with_func_group_labels.jpg",sp_freq_plot_CT,height=8,width=11)


#######
# B&W #
#######
# re-order functional groups so it goes from light to dary on the legend 
dataSPECIES_freq3$group <- factor(dataSPECIES_freq3$group, levels=c("floating", "submerged", "emergent", "lily"))


sp_freq_plot_CT <- ggplot(dataSPECIES_freq3,aes(x=reorder(species, -frequency),y=frequency,fill=group))
sp_freq_plot_CT <- sp_freq_plot_CT + geom_bar(aes(fill=group),stat="identity",colour="black")
################################################################ floating, submerged, emergent, lily
sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("grey90","grey55","grey25","black"))
################################################################ emergent, floating, lily, submerged
#sp_freq_plot_CT <- sp_freq_plot_CT + scale_fill_manual(values=c("grey28","grey90","black","grey50"))
sp_freq_plot_CT <- sp_freq_plot_CT + xlab("Species")
sp_freq_plot_CT <- sp_freq_plot_CT + ylab("# of waterbodies found in")
sp_freq_plot_CT <- sp_freq_plot_CT + geom_text(aes(y=frequency+1,label=Label))
sp_freq_plot_CT <- sp_freq_plot_CT + ggtitle("Connecticut, USA (n=174)")
sp_freq_plot_CT <- sp_freq_plot_CT + theme_classic()
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + scale_x_discrete(labels=seq(1,nrow(dataSPECIES_freq2),1)) # replace species names with #s
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.x = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.title.y = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(plot.title = element_text(size=18))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.y = element_text(size=14))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(size=10))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(axis.text.x = element_text(angle = 0, hjust = 1))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.title = element_text(size=16, face="bold"))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.text = element_text(size = 12))
sp_freq_plot_CT <- sp_freq_plot_CT + theme(legend.position=c(.95, .25))
sp_freq_plot_CT

ggsave("sp_freq_plot_CT - with_func_group_labels - B&W.jpg",sp_freq_plot_CT,height=8,width=11)


############################
# Table of FP compositions # 
############################
table_dataFP_comp <- ddply(dataFP,.(azolla,eichhornia_crassipes,lemna_minor,lemna_trisulca,spirodela_polyrhiza,wolffia),nrow)
colnames(table_dataFP_comp)[7]<-"frequency"
table_dataFP_comp

# Copy & paste this into Excel for further formatting

#########################
# Histograms of dataENV #
#########################
dataENV$id <- 1:nrow(dataENV)

# re-arranged the data 
d <- melt(dataENV[,1:ncol(dataENV)],id.vars="id")

# Untransformed 
hist_dataENV <- ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
hist_dataENV <- hist_dataENV + theme_bw(base_size=18)
hist_dataENV
ggsave("hist_dataENV.jpg",hist_dataENV,height=11,width=8)

# Log-transformed 
hist_log_dataENV <- ggplot(d,aes(x = log(value+0.1))) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
hist_log_dataENV <- hist_log_dataENV + theme_bw(base_size=18)
hist_log_dataENV
ggsave("hist_log_dataENV.jpg",hist_log_dataENV,height=11,width=8)

rm(d)

##################################
# LEMNA MINOR on x-axis          #
# Continuous variables on y-axis #
##################################
# subset the data frame "data" for each species 
dataLEMNA <- data[c("lemna_minor",
                   "surfacearea_ha","shoreline_development",
                   "depth_max_m","nonFP_species_richness",
                   "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                   "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                   "nearest_LM","nearest_SP","nearest_W","nearest_any_FP")]

for(i in names(dataLEMNA[,2:ncol(dataLEMNA)])){
  plots <- ggplot(data,aes_string(x="lemna_minor",y=i)) + geom_point(position=position_jitter(w=0.1,h=0))  
  plots <- plots + xlab("Lemna minor")
  plots <- plots + theme_bw(base_size=18)
  plots <- plots + scale_x_continuous(breaks=0:1,labels=c("Absent","Present"))
  ggsave(plots,filename=paste("lemna_and_",i,".jpg",sep=""))
}

##################################
# SPIRODELA POLYRHIZA on x-axis  #
# Continuous variables on y-axis #
##################################
# subset the data frame "data" for each species 
dataSPIRO <- data[c("spirodela_polyrhiza",
                    "surfacearea_ha","shoreline_development",
                    "depth_max_m","nonFP_species_richness",
                    "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                    "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                    "nearest_LM","nearest_SP","nearest_W","nearest_any_FP")]

for(i in names(dataSPIRO[,2:ncol(dataSPIRO)])){
  plots <- ggplot(data,aes_string(x="spirodela_polyrhiza",y=i)) + geom_point(position=position_jitter(w=0.1,h=0))  
  plots <- plots + xlab("Spirodela polyrhiza")
  plots <- plots + theme_bw(base_size=18)
  plots <- plots + scale_x_continuous(breaks=0:1,labels=c("Absent","Present"))
  ggsave(plots,filename=paste("spirodela_and_",i,".jpg",sep=""))
}


##################################
# WOLFFIA SP. on x-axis          #
# Continuous variables on y-axis #
##################################
# subset the data frame "data" for each species 
dataWOLFFIA <- data[c("wolffia_sp",
                    "surfacearea_ha","shoreline_development",
                    "depth_max_m","nonFP_species_richness",
                    "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                    "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                    "nearest_LM","nearest_SP","nearest_W","nearest_any_FP")]

for(i in names(dataWOLFFIA[,2:ncol(dataWOLFFIA)])){
  plots <- ggplot(data,aes_string(x="wolffia_sp",y=i)) + geom_point(position=position_jitter(w=0.1,h=0))  
  plots <- plots + xlab("Wolffia sp.")
  plots <- plots + theme_bw(base_size=18)
  plots <- plots + scale_x_continuous(breaks=0:1,labels=c("Absent","Present"))
  ggsave(plots,filename=paste("Wolffia_and_",i,".jpg",sep=""))
}


##################################
# FP presence on x-axis          #
# Continuous variables on y-axis #
##################################
# subset the data frame "data" for each species 
dataFPpres <- data[c("FP_presence",
                      "surfacearea_ha","shoreline_development",
                      "depth_max_m","nonFP_species_richness",
                      "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                      "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                      "nearest_LM","nearest_SP","nearest_W","nearest_any_FP")]

for(i in names(dataFPpres[,2:ncol(dataFPpres)])){
  plots <- ggplot(data,aes_string(x="FP_presence",y=i)) + geom_point(position=position_jitter(w=0.1,h=0))  
  plots <- plots + xlab("FP presence")
  plots <- plots + theme_bw(base_size=18)
  plots <- plots + scale_x_continuous(breaks=0:1,labels=c("Absent","Present"))
  ggsave(plots,filename=paste("FP_presence_and_",i,".jpg",sep=""))
}


##################################
# FP richness on x-axis          #
# Continuous variables on y-axis #
##################################
# subset the data frame "data" 
dataFPrich <- data[c("FP_species_richness",
                      "surfacearea_ha","shoreline_development",
                      "depth_max_m","nonFP_species_richness",
                      "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                      "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                      "nearest_LM","nearest_SP","nearest_W","nearest_any_FP")]

for(i in names(dataFPrich[,2:ncol(dataFPrich)])){
  plots <- ggplot(data,aes_string(x="FP_species_richness",y=i)) + geom_point(position=position_jitter(w=0.1,h=0))  
  plots <- plots + xlab("FP species richness")
  plots <- plots + theme_classic(base_size=18)
  plots <- plots + scale_x_continuous(breaks=0:4)
  ggsave(plots,filename=paste("FPrichness_and_",i,".jpg",sep=""))
}





# This analysis below was done August 2014
# But not repeated in December 2014

############################ 
# watersheds on the x-axis # 
# Y = LEMNA MINOR          #
############################
# major watershed 
lemna_and_watershed <- ggplot(data,aes(x=major_watershed,y=lemna_minor)) 
lemna_and_watershed <- lemna_and_watershed + geom_point(position=position_jitter(w=0.1,h=0.1)) 
lemna_and_watershed <- lemna_and_watershed + ylab("Lemna minor")
lemna_and_watershed <- lemna_and_watershed + xlab("Major watershed")
lemna_and_watershed <- lemna_and_watershed + scale_y_continuous(breaks=0:1,labels=c("Absent","Present"))
lemna_and_watershed <- lemna_and_watershed + theme_classic(base_size=18)
lemna_and_watershed <- lemna_and_watershed + theme(axis.text.x = element_text(angle=90,hjust=1))
lemna_and_watershed
ggsave(lemna_and_watershed,filename="lemna_and_watershed.jpg")

# regional watershed 
# Lemna will be the only species with "enough" data for the # of regional watershed categories
lemna_and_watershed_2 <- ggplot(data,aes(x=regional_watershed,y=lemna_minor)) 
lemna_and_watershed_2 <- lemna_and_watershed_2 + geom_point(position=position_jitter(w=0.1,h=0.1)) 
lemna_and_watershed_2 <- lemna_and_watershed_2 + ylab("Lemna minor")
lemna_and_watershed_2 <- lemna_and_watershed_2 + xlab("Regional watershed")
lemna_and_watershed_2 <- lemna_and_watershed_2 + scale_y_continuous(breaks=0:1,labels=c("Absent","Present"))
lemna_and_watershed_2 <- lemna_and_watershed_2 + theme_classic(base_size=18)
lemna_and_watershed_2 <- lemna_and_watershed_2 + theme(axis.text.x = element_text(angle=90,hjust=1))
lemna_and_watershed_2
ggsave(lemna_and_watershed_2,filename="lemna_and_watershed_2.jpg")


############################ 
# watersheds on the x-axis # 
# Y = SPIRODELA POLYRHIZA  #
############################
spirodela_and_watershed <- ggplot(data,aes(x=major_watershed,y=spirodela_polyrhiza)) 
spirodela_and_watershed <- spirodela_and_watershed + geom_point(position=position_jitter(w=0.1,h=0.1)) 
spirodela_and_watershed <- spirodela_and_watershed + ylab("Spirdela polyrhiza")
spirodela_and_watershed <- spirodela_and_watershed + xlab("Major watershed")
spirodela_and_watershed <- spirodela_and_watershed + scale_y_continuous(breaks=0:1,labels=c("Absent","Present"))
spirodela_and_watershed <- spirodela_and_watershed + theme_classic(base_size=18)
spirodela_and_watershed <- spirodela_and_watershed + theme(axis.text.x = element_text(angle=90,hjust=1))
spirodela_and_watershed
ggsave(spirodela_and_watershed,filename="spirodela_and_watershed.jpg")

############################ 
# watersheds on the x-axis # 
# WOLFFIA SP.              #
############################
wolffia_and_watershed <- ggplot(data,aes(x=major_watershed,y=wolffia_sp)) 
wolffia_and_watershed <- wolffia_and_watershed + geom_point(position=position_jitter(w=0.1,h=0.1)) 
wolffia_and_watershed <- wolffia_and_watershed + ylab("Wolffia sp.")
wolffia_and_watershed <- wolffia_and_watershed + xlab("Major watershed")
wolffia_and_watershed <- wolffia_and_watershed + scale_y_continuous(breaks=0:1,labels=c("Absent","Present"))
wolffia_and_watershed <- wolffia_and_watershed + theme_classic(base_size=18)
wolffia_and_watershed <- wolffia_and_watershed + theme(axis.text.x = element_text(angle=90,hjust=1))
wolffia_and_watershed
ggsave(wolffia_and_watershed,filename="wolffia_and_watershed.jpg")


############################ 
# watersheds on the x-axis # 
# FPrichness               #
############################
FPrich_and_watershed <- ggplot(data,aes(x=major_watershed,y=FP_species_richness)) 
FPrich_and_watershed <- FPrich_and_watershed + geom_point(position=position_jitter(w=0.1,h=0.1)) 
FPrich_and_watershed <- FPrich_and_watershed + ylab("FP species richness")
FPrich_and_watershed <- FPrich_and_watershed + xlab("Major watershed")
FPrich_and_watershed <- FPrich_and_watershed + scale_y_continuous(breaks=0:4)
FPrich_and_watershed <- FPrich_and_watershed + theme_classic(base_size=18)
FPrich_and_watershed <- FPrich_and_watershed + theme(axis.text.x = element_text(angle=90,hjust=1))
FPrich_and_watershed
ggsave(FPrich_and_watershed,filename="FPrich_and_watershed.jpg")


############################ 
# watersheds on the x-axis # 
# Y = environmental vars.  #
############################
# subset the data frame "data" 
dataWATERSHED <- data[c("regional_watershed",
                     "surfacearea_ha","shoreline_development",
                     "depth_max_m","nonFP_species_richness",
                     "TOTP_avg","PH_avg","COND_avg","ALK_avg","secchi_avg",
                     "waterbodies_1km","waterbodies_5km","waterbodies_10km","dist_waterfowl",
                     "nearest_LM","nearest_SP","nearest_W")]

for(i in names(dataWATERSHED[,2:ncol(dataWATERSHED)])){
  plots <- ggplot(data,aes_string(x="regional_watershed",y=i)) + geom_point(aes(alpha=0.5))  
  plots <- plots + xlab("Regional wateshed")
  plots <- plots + theme_classic(base_size=14)
  plots <- plots + theme(legend.position="none")
  plots <- plots + theme(axis.text.x = element_text(angle=90,hjust=1))
  ggsave(plots,filename=paste("watershed_and_",i,".jpg",sep=""),height=5,width=12)
}