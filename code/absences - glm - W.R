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

# combine the wolffias 
# data$wolffia_sp <- data$wolffia + data$wolffia_borealis + data$wolffia_brasiliensis 
# already done 

# number of water bodies with wolffia present 
sum(data$wolffia_sp)

######################################
# Define a full model                #
# not included: depth_max_m, ALK_avg #
# No interactions                    #
######################################
formula <- wolffia_sp ~ log(surfacearea_ha) + log(shoreline_development) + log(nonFP_species_richness+1) + 
                          log(TOTP_avg) + log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
                          log(waterbodies_1km+1) + log(waterbodies_10km) + 
                          boatlaunch + log(dist_waterfowl+1)
glm_binomial_W_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_W_full_log)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_W_log <- dredge(glm_binomial_W_full_log)
best_W_log

numb_models <- nrow(best_W_log)
write.csv(best_W_log[1:numb_models],file="best_glm_W_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_W_log, 1)

# Visualize the model selection table:
plot(best_W_log)

# Model average models with delta AICc < 2
avg_W_log <- model.avg(best_W_log, subset = delta < 2)
summary(avg_W_log)
avg_W_log

# Alternatively, return standarzed coefficients 
model.avg(best_W_log, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_W_log)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_W_log)
