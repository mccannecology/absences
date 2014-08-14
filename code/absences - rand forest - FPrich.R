###########################
# Random forest           #
# FP richness             #
###########################
library(randomForest)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km 
# Exclued regional_watershed (cannot handle > 32 categories)
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- FP_species_richness ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + nonFP_species_richness +
  major_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_LM + nearest_SP + nearest_W

# fit the model 
FPrich.randForest <- randomForest(formula, data=data, importance=T, ntree=5000)

# sumamry of the model 
print(FPrich.randForest)

# variable importance 
importance(FPrich.randForest)

# plot variable importance and save it
jpeg(file="rand_forest_FPrich.jpg",height=8,width=11,units="in",res=150)
varImpPlot(FPrich.randForest)
dev.off()




