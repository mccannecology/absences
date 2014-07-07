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

###################
# Regression tree #
# Y = wolffia     #
###################
tree_wolffia <- tree(as.factor(wolffia) ~ surfacearea_ha+shoreline_development+TOTP_avg+PH_avg+COND_avg+secchi_avg+NONFPrichness, split = "deviance", data = dataTREE)
tree_wolffia

jpeg("tree_wolffia.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_wolffia, main = "tree_wolffia") 
text(tree_wolffia, all=TRUE, cex = 0.75) 
title("tree_wolffia")
dev.off() 

cv_tree_wolffia <- cv.tree(tree_wolffia) 
jpeg("cv_tree_wolffia.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(cv_tree_wolffia, main="cv_tree_wolffia")
dev.off() 

## Plot the pruned tree ##
best_tree_size <- cv_tree_wolffia$size[which.min(cv_tree_wolffia$dev)]
if (best_tree_size < 2) {
  best_tree_size <- 2
  print("Best tree size <2")
}
best_tree_size <- 4
tree_wolffia_pruned <- prune.tree(tree_wolffia, best=best_tree_size)
jpeg("tree() - FPRICH - all ponds - integer - pruned.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_wolffia_pruned, main = "tree() - FPRICH - all ponds - pruned") 
text(tree_wolffia_pruned, all=TRUE, cex = 0.75) 
title("tree_wolffia_pruned")
dev.off() 
