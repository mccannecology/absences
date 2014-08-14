##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
#                                            #
# Presence / Absence of floating plants      # 
# Importing data sets                        #
# Created: 07/14/2014 by MJ McCann           #
##############################################

data
dataENV
dataFP
dataNONFP
dataSPACE
dataSPECIES

head(data)

# number of water bodies with LM present 
sum(data$lemna_minor)

######################################
# Define a full model                #
# Excluded:                          #
# -depth_max_m                       #
# -ALK_avg                           #  
# -waterbodies_5km                   # 
# -nearest_SP                        #
# -nearest_W                         #
# -nearest_LMSPW                     #
# No interactions                    #
######################################
formula <- lemna_minor ~ log(surfacearea_ha) + log(shoreline_development) + log(nonFP_species_richness+1) + 
                            log(TOTP_avg) + log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
                            log(waterbodies_1km+1) + log(waterbodies_10km) + 
                            boatlaunch + log(dist_waterfowl+1) + log(nearest_LM) #+ regional_watershed
glm_binomial_LM_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_LM_full_log)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_LM_log <- dredge(glm_binomial_LM_full_log)
best_LM_log

numb_models <- nrow(best_LM_log)
write.csv(best_LM_log[1:numb_models],file="best_glm_LM_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_LM_log, 1)

# Visualize the model selection table:
plot(best_LM_log)

# Model average models with delta AICc < 4
avg_LM_log <- model.avg(best_LM_log, subset = delta < 2)
summary(avg_LM_log)
avg_LM_log

# Alternatively, return standarzed coefficients 
model.avg(best_LM_log, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_LM_log)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_LM_log)