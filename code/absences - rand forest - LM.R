###########################
# Random forest           #
# Lemna minor             # 
###########################
library(randomForest)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km, nearest_SP, nearest_W
# Exclued regional_watershed (cannot handle > 32 categories)
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- as.factor(lemna_minor) ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + nonFP_species_richness +
  major_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_LM 

# fit the model 
LM.randForest <- randomForest(formula, data=data, importance=T, ntree=5000)

# sumamry of the model 
print(LM.randForest)

# variable importance 
importance(LM.randForest)

# plot variable importance and save it
jpeg(file="rand_forest_LM.jpg",height=8,width=11,units="in",res=150)
varImpPlot(LM.randForest)
dev.off()




