###########################
# Random forest           #
# Wolffia sp.             #
###########################
library(randomForest)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5k, nearest_LM, nearest_SP
# Exclued regional_watershed (cannot handle > 32 categories)
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- as.factor(wolffia_sp) ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + nonFP_species_richness +
  major_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_W

# fit the model 
W.randForest <- randomForest(formula, data=data, importance=T, ntree=5000)

# sumamry of the model 
print(W.randForest)

# variable importance 
importance(W.randForest)

# plot variable importance and save it
jpeg(file="rand_forest_W.jpg",height=8,width=11,units="in",res=150)
varImpPlot(W.randForest)
dev.off()




