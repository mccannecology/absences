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
# not included: depth_max_m, ALK_avg #
# No interactions                    #
######################################
formula <- lemna_minor ~ surfacearea_ha + shoreline_development + nonFP_species_richness+ TOTP_avg + PH_avg + COND_avg + secchi_avg
glm_binomial_LM_full <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_LM_full)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_LM <- dredge(glm_binomial_LM_full)
best_LM

numb_models <- nrow(best_LM)
write.csv(best_LM[1:numb_models],file="best_LM.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_LM, 1)

# Visualize the model selection table:
plot(best_LM)

# Model average models with delta AICc < 4
avg_LM <- model.avg(best_LM, subset = delta < 2)
summary(avg_LM)
avg_LM

# Alternatively, return standarzed coefficients 
model.avg(best_LM, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_LM)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_LM)


#################
# Plot          #  
# X = Variable  #
# Y = Predicted #
#################