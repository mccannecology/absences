###########################
# Cond. Inference Tree    #
# Wolffia sp.             #
###########################
library(party)

# Check out the column names to find out where your variables are 
colnames(data)

# this cannot have any missing data - so check here 
summary(data)

# Exclude waterbodies_5km, nearest_LM, nearest_SP 
# Include depth_max_m and ALK_avg

# define your model formula 
formula <- wolffia_sp ~ latitude + longitude + surfacearea_ha + shoreline_development +
  depth_max_m + nonFP_species_richness + TOTP_avg + PH_avg + COND_avg + ALK_avg + secchi_avg + 
  major_watershed + regional_watershed + waterbodies_1km + waterbodies_10km + boatlaunch + 
  dist_waterfowl + nearest_W

# fit the model 
W.ctree <- ctree(formula, data=data, controls = ctree_control())  

# plot the model 
jpeg(file="cond_inf_tree_W.jpg")
plot(W.ctree, main = "Cond. Inf. Tree: Wolffia sp.",terminal_panel=node_hist(W.ctree,breaks=0:2-0.5,ymax=125,horizontal=FALSE,freq=TRUE))
dev.off()

