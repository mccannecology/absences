library(vegan)

?varpart

colnames(data)
colnames(dataENV_trans)
colnames(dataFP)

###################### 
# Response variables #
######################
data_LM <- dataFP$lemna_minor
data_SP <- dataFP$spirodela_polyrhiza
data_W <- dataFP$wolffia
data_FPpres <- data$FP_presence
data_FPrich <- data$FP_species_richness

#########
# Local #
#########
local <- dataENV_trans[,c('surfacearea_ha','shoreline_development','depth_max_m','TOTP_avg','PH_avg',
                            'COND_avg','ALK_avg','secchi_avg','nonFP_species_richness')]
###########
# Spatial #
###########
spatial <- dataENV_trans[,c('latitude','longitude')]

#############
# Dispersal #
#############
dispersal_LM <- dataENV_trans[,c('waterbodies_1km','waterbodies_10km','dist_waterfowl','nearest_LM',
                                 'boatlaunch')]
dispersal_SP <- dataENV_trans[,c('waterbodies_1km','waterbodies_10km','dist_waterfowl','nearest_SP',
                                 'boatlaunch')]
dispersal_W <- dataENV_trans[,c('waterbodies_1km','waterbodies_10km','dist_waterfowl','nearest_W',
                                 'boatlaunch')]
dispersal_FPpres <- dataENV_trans[,c('waterbodies_1km','waterbodies_10km','dist_waterfowl','nearest_LM',
                                  'nearest_SP','nearest_W','boatlaunch')]
dispersal_FPrich <- dataENV_trans[,c('waterbodies_1km','waterbodies_10km','dist_waterfowl','nearest_LM',
                                     'nearest_SP','nearest_W','boatlaunch')]

#########################
# Variance partitioning # 
######################### 
var_part_LM <- varpart(data_LM,local,spatial,dispersal_LM)
var_part_SP <- varpart(data_SP,local,spatial,dispersal_SP)
var_part_W <- varpart(data_W,local,spatial,dispersal_W)
var_part_FPpres <- varpart(data_FPpres,local,spatial,dispersal_FPpres)
var_part_FPrich <- varpart(data_FPrich,local,spatial,dispersal_FPrich)

