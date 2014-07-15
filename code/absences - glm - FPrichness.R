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

######################################
# Define a full model                #
# not included: depth_max_m, ALK_avg #
# No interactions                    #
######################################
formula <- FP_species_richness ~ surfacearea_ha + shoreline_development + nonFP_species_richness+ TOTP_avg + PH_avg + COND_avg + secchi_avg
glm_poisson_FPrich_full <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_full)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_FPrich <- dredge(glm_poisson_FPrich_full)
numb_models <- nrow(best_FPrich)
write.csv(best_FPrich[1:numb_models],file="best_FPrich.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_FPrich, 1)

# print all model outputs
get.models(best_FPrich)

# Visualize the model selection table:
plot(best_FPrich)

# Model average models with delta AICc < 4
avg_FPrich <- model.avg(best_FPrich, subset = delta < 4)
summary(avg_FPrich)
avg_FPrich

# or as a 95% confidence set:
avg_FPrich_95p <- model.avg(best_FPrich, subset = cumsum(weight) <= .95) 
confint(avg_FPrich_95p)

# returns standarzed coefficients 
model.avg(best_FPrich, subset = delta < 4, beta = TRUE)

# returns coefficients 
coef(avg_FPrich)

# can only predict from 'averaging' object created with a model list
predict(avg_FPrich)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_FPrich)


#################
# Plot          #  
# X = Variable  #
# Y = Predicted #
#################