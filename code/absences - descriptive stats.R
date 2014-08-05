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
plot(data$dist_waterfowl,data$FP_species_richness,ylab="Floating plant species richness",xlab="Distance to migratory waterfowl habitat (km)")
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
plot(data$dist_waterfowl,data$lemna_minor,ylab="Lemna minor",xlab="Distance to migratory waterfowl habitat (km)")
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
plot(data$dist_waterfowl,data$spirodela_polyrhiza,ylab="Spirodela polyrhiza",xlab="Distance to migratory waterfowl habitat (km)")
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
plot(data$dist_waterfowl,data$wolffia_sp,ylab="Wolffia sp",xlab="Distance to migratory waterfowl habitat (km)")
dev.off()

#############################
# Histogram of sp. richness #
# Floating plants           #
#############################
FP_richness_vector <- as.data.frame(rowSums(dataFP))
colnames(FP_richness_vector)[1]<-"richness"
FP_richness_vector
table(FP_richness_vector)
FP_richness_matrix <- matrix(c(0,1,2,3,4,102,39,21,11,1),nrow=5,ncol=2,byrow=F)
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
#################################
head(dataSPECIES_freq)

# if you want this plot with species listed alphabetically,
# re-do dataSPECIES_freq in "absences - import.R" 
# and leave out ordering the species as a factor of frequency 
          
dataSPECIES_freq2 <- read.csv("dataSPECIES_freq.csv")

sp_freq_plot <- ggplot(dataSPECIES_freq2,aes(x=reorder(Species, -Frequency),y=Frequency,fill=Label))
sp_freq_plot <- sp_freq_plot + geom_bar(stat="identity")
sp_freq_plot <- sp_freq_plot + theme_classic(base_size=10)
sp_freq_plot <- sp_freq_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot <- sp_freq_plot + xlab("Species")
sp_freq_plot <- sp_freq_plot + ylab("# of waterbodies found in")
sp_freq_plot <- sp_freq_plot + geom_text(aes(y=Frequency+1,label=Label))
sp_freq_plot <- sp_freq_plot + ggtitle("This study - 174 lakes and ponds Connecticut, USA")
sp_freq_plot <- sp_freq_plot + theme(legend.position="none")
sp_freq_plot

ggsave("sp_freq_plot2.jpg",sp_freq_plot,height=8,width=11)

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
# re-arranged the data 
d <- melt(dataENV[,1:ncol(dataENV)])

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
