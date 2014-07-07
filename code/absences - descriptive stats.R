library(ggplot2)
library(reshape2)
library(plyr)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

################
# Scatterplots #
################
plot(data$TOTP_avg,data$FPrichness,log="x",ylab="Floating plant species richness",xlab="Total P(mg/L)")

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
                                          
sp_freq_plot <- ggplot(dataSPECIES_freq,aes(x=species,y=frequency))
sp_freq_plot <- sp_freq_plot + geom_bar(stat="identity",fill = I("grey50"))
sp_freq_plot <- sp_freq_plot + theme_classic(base_size=10)
sp_freq_plot <- sp_freq_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sp_freq_plot <- sp_freq_plot + xlab("Species")
sp_freq_plot <- sp_freq_plot + ylab("# of waterbodies found in")
sp_freq_plot

ggsave("sp_freq_plot.jpg",sp_freq_plot,height=8,width=11)

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
hist_log_dataENV <- ggplot(d,aes(x = log(value))) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
hist_log_dataENV <- hist_log_dataENV + theme_bw(base_size=18)
hist_log_dataENV
ggsave("hist_log_dataENV.jpg",hist_log_dataENV,height=11,width=8)
