###########################
# Cond. Inference Tree    #
# All species richness    #
###########################
library(party)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km 
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- allplant_species_richness ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + 
  major_watershed + regional_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_LM + nearest_SP + nearest_W

# fit the model 
ALrich.ctree <- ctree(formula, data=data, controls = ctree_control())  

# plot the model 
jpeg(file="cond_inf_tree_ALLrich.jpg")
plot(ALrich.ctree, main = "Cond. Inf. Tree: All plant  species richness",terminal_panel=node_hist(ALrich.ctree,breaks=0:45-0.5,horizontal=FALSE,freq=TRUE))
dev.off()
