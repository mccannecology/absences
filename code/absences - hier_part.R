library(hier.part)

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

####################### 
# Predictor variables #
#######################
pred_LM <- dataENV_trans[,c('surfacearea_ha','shoreline_development','TOTP_avg','PH_avg',
                            'COND_avg','secchi_avg','waterbodies_1km','dist_waterfowl','nearest_LM',
                            'boatlaunch','nonFP_species_richness')]

pred_SP <- dataENV_trans[,c('surfacearea_ha','shoreline_development','TOTP_avg','PH_avg',
                            'COND_avg','secchi_avg','waterbodies_1km','dist_waterfowl','nearest_SP',
                            'boatlaunch','nonFP_species_richness')] 

pred_W <- dataENV_trans[,c('surfacearea_ha','shoreline_development','TOTP_avg','PH_avg',
                          'COND_avg','secchi_avg','waterbodies_1km','dist_waterfowl','nearest_W',
                          'boatlaunch','nonFP_species_richness')]  

pred_FPpres <- dataENV_trans[,c('surfacearea_ha','shoreline_development','TOTP_avg','PH_avg',
                                      'COND_avg','secchi_avg','waterbodies_1km','dist_waterfowl','nearest_LMSPW',
                                      'boatlaunch','nonFP_species_richness')]   

pred_FPrich <- dataENV_trans[,c('surfacearea_ha','shoreline_development','TOTP_avg','PH_avg',
                                'COND_avg','secchi_avg','waterbodies_1km','dist_waterfowl','nearest_LMSPW',
                                'boatlaunch','nonFP_species_richness')]  


#############################
# Hierarchical partitioning # 
# LM                        # 
#############################
hier_part_LM_01 <- hier.part(data_LM, pred_LM, family="binomial")
hier_part_LM_02 <- hier.part(data_LM, cbind(pred_LM[,1:6],pred_LM[,7:ncol(pred_LM)]), family="binomial")
hier_part_LM_03 <- hier.part(data_LM, cbind(pred_LM[,4:6],pred_LM[,10:ncol(pred_LM)],pred_LM[,1:3],pred_LM[,7:9]), family="binomial")

# see how the order of variables affects the results 
hier_part_LM_01$I.perc
hier_part_LM_02$I.perc
hier_part_LM_03$I.perc
