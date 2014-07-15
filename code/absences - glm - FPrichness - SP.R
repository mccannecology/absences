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
formula <- spirodela_polyrhiza ~ surfacearea_ha + shoreline_development + nonFP_species_richness+ TOTP_avg + PH_avg + COND_avg + secchi_avg
glm_binomial_SP_full <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_SP_full)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_SP <- dredge(glm_binomial_SP_full)
best_SP
numb_models <- nrow(best_SP)
write.csv(best_SP[1:numb_models],file="best_SP.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_SP, 1)

# Visualize the model selection table:
plot(best_SP)

# Model average models with delta AICc < 2
avg_SP <- model.avg(best_SP, subset = delta < 2)
summary(avg_SP)
avg_SP

# Alternatively, return standarzed coefficients 
model.avg(best_SP, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_SP)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_SP)


#################
# Plot          #  
# X = Variable  #
# Y = Predicted #
#################