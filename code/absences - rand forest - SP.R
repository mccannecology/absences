###########################
# Random forest           #
# Spirodela polyrhiza     #
###########################
library(randomForest)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km, nearest_LM, nearest_W
# Exclued regional_watershed (cannot handle > 32 categories)
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- as.factor(spirodela_polyrhiza) ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + nonFP_species_richness +
  major_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_SP 

# fit the model 
SP.randForest <- randomForest(formula, data=data, importance=T, ntree=5000)

# sumamry of the model 
print(SP.randForest)

# variable importance 
importance(SP.randForest)

# plot variable importance and save it
jpeg(file="rand_forest_SP.jpg",height=8,width=11,units="in",res=150)
varImpPlot(SP.randForest)
dev.off()




