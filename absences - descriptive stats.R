library(ggplot2)
library(reshape2)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

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
