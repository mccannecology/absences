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
# Y = FPrichness  #
# package tree    #
###################
tree_FPrichness <- tree(as.factor(FPrichness) ~ surfacearea_ha+shoreline_development+TOTP_avg+PH_avg+COND_avg+secchi_avg+NONFPrichness, split = "deviance", data = dataTREE)
tree_FPrichness

jpeg("tree_FPrichness.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_FPrichness, main = "tree_FPrichness") 
text(tree_FPrichness, all=TRUE, cex = 0.75) 
title("tree_FPrichness")
dev.off() 

cv_tree_FPrichness <- cv.tree(tree_FPrichness) 
jpeg("cv_tree_FPrichness.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(cv_tree_FPrichness, main="cv_tree_FPrichness")
dev.off() 

## Plot the pruned tree ##
best_tree_size <- cv_tree_FPrichness$size[which.min(cv_tree_FPrichness$dev)]
if (best_tree_size < 2) {
  best_tree_size <- 2
  print("Best tree size <2")
}
tree_FPrichness_pruned <- prune.tree(tree_FPrichness, best=best_tree_size)
jpeg("tree() - FPRICH - all ponds - integer - pruned.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_FPrichness_pruned, main = "tree() - FPRICH - all ponds - pruned") 
text(tree_FPrichness_pruned, all=TRUE, cex = 0.75) 
title("tree_FPrichness_pruned")
dev.off() 

###################
# Regression tree #
# Y = FPrichness  #
# package rpart   #
###################
rpart_FPrichness <- rpart(FPrichness ~ surfacearea_ha+shoreline_development+TOTP_avg+PH_avg+COND_avg+secchi_avg+NONFPrichness, data = dataTREE)
rpart_FPrichness

plot(rpart_FPrichness)
text(rpart_FPrichness,use.n=TRUE,cex=0.75)

plotcp(rpart_FPrichness, main='rpart_FPrichness')

# prune the tree 
rpart_FPrichness_pruned <- prune(rpart_FPrichness, cp = rpart_FPrichness$cptable[which.min(rpart_FPrichness$cptable[,"xerror"]),"CP"])
plot(rpart_FPrichness_pruned, main = "rpart_FPrichness_pruned") 
text(rpart_FPrichness_pruned, use.n=TRUE, cex=0.75)

###################
# Regression tree #
# Y = FPrichness  #
# package ctree   #
###################
ctree_FPrichness <- ctree(FPrichness ~ surfacearea_ha+shoreline_development+TOTP_avg+PH_avg+COND_avg+secchi_avg+NONFPrichness, data = dataTREE)
ctree_FPrichness

plot(ctree_FPrichness, main = "ctree_FPrichness") 
