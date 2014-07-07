library(tree)
library(rpart)
library(party) 

#########################
# data frames available #
#########################
data
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

names(data)

#############################
# Make data frame for trees #
#############################
# add NONFP richness as a value 
data$NONFPrichness <- rowSums(dataNONFP)

# add NONFP richness as a value 
data$FPrichness <- rowSums(dataFP)

# remove NONFP presence/absence 
dataTREE <- data[,-c(seq(36,152,))]
colnames(dataTREE)

# remove wolffia_brasiliensis & wolffia_borealis
dataTREE <- dataTREE[,-26]
dataTREE <- dataTREE[,-26]
# assign wolffia from dataFP 
dataTREE$wolffia <- dataFP$wolffia

#####################
# Regression tree   #
# Y = NONFPrichness #
# package tree      #
#####################
