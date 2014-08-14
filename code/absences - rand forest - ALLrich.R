###########################
# Random forest           #
# All species richness    #
###########################
library(randomForest)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km, nonFP_species_richness
# Exclued regional_watershed (cannot handle > 32 categories)
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- allplant_species_richness ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + 
  major_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_LM + nearest_SP + nearest_W

# fit the model 
ALLrich.randForest <- randomForest(formula, data=data, importance=T, ntree=5000)

# sumamry of the model 
print(ALLrich.randForest)

# variable importance 
importance(ALLrich.randForest)

# plot variable importance and save it
jpeg(file="rand_forest_ALLrich.jpg",height=8,width=11,units="in",res=150)
varImpPlot(ALLrich.randForest)
dev.off()




