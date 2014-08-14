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

# number of water bodies with SP present 
sum(data$spirodela_polyrhiza)

######################################
# Define a full model                #
# not included: depth_max_m, ALK_avg #
# No interactions                    #
######################################
formula <- spirodela_polyrhiza ~ log(surfacearea_ha) + log(shoreline_development) + log(nonFP_species_richness+1) + 
                                    log(TOTP_avg) + log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
                                    log(waterbodies_1km+1) + log(waterbodies_10km) + 
                                    boatlaunch + log(dist_waterfowl+1)
glm_binomial_SP_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_SP_full_log)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_SP__log <- dredge(glm_binomial_SP_full_log)
best_SP__log
numb_models <- nrow(best_SP__log)
write.csv(best_SP__log[1:numb_models],file="best_glm_SP_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_SP__log, 1)

# Visualize the model selection table:
plot(best_SP__log)

# Model average models with delta AICc < 2
avg_SP__log <- model.avg(best_SP__log, subset = delta < 2)
summary(avg_SP__log)
avg_SP__log

# Alternatively, return standarzed coefficients 
model.avg(best_SP__log, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_SP__log)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_SP__log)

