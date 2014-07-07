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
# Y = lemna_minor #
###################
tree_lemna_minor <- tree(as.factor(lemna_minor) ~ surfacearea_ha+shoreline_development+TOTP_avg+PH_avg+COND_avg+secchi_avg+NONFPrichness, split = "deviance", data = dataTREE)
tree_lemna_minor

jpeg("tree_lemna_minor.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_lemna_minor, main = "tree_lemna_minor") 
text(tree_lemna_minor, all=TRUE, cex = 0.75) 
title("tree_lemna_minor")
dev.off() 

cv_tree_lemna_minor <- cv.tree(tree_lemna_minor) 
jpeg("cv_tree_lemna_minor.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(cv_tree_lemna_minor, main="cv_tree_lemna_minor")
dev.off() 

## Plot the pruned tree ##
best_tree_size <- cv_tree_lemna_minor$size[which.min(cv_tree_lemna_minor$dev)]
if (best_tree_size < 2) {
  best_tree_size <- 2
  print("Best tree size <2")
}
tree_lemna_minor_pruned <- prune.tree(tree_lemna_minor, best=best_tree_size)
jpeg("tree() - FPRICH - all ponds - integer - pruned.jpg", width = 1200, height = 1200, units = "px", quality = 100, res=125)
plot(tree_lemna_minor_pruned, main = "tree() - FPRICH - all ponds - pruned") 
text(tree_lemna_minor_pruned, all=TRUE, cex = 0.75) 
title("tree_lemna_minor_pruned")
dev.off() 
