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
formula <- FP_species_richness ~ surfacearea_ha + shoreline_development + nonFP_species_richness + TOTP_avg + PH_avg + COND_avg + secchi_avg
glm_poisson_FPrich_full <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_full)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)

# run all possible sub-models 
best_FPrich <- dredge(glm_poisson_FPrich_full)
best_FPrich

# write the table of all sub-models to file 
numb_models <- nrow(best_FPrich)
write.csv(best_FPrich[1:numb_models],file="best_FPrich.csv",na="NA")

# Return the best model (lowest AIC)
get.models(best_FPrich, 1)

# Visualize the model selection table:
plot(best_FPrich)

# Model average models with delta AICc < 2
avg_FPrich <- model.avg(best_FPrich, subset = delta < 2)
summary(avg_FPrich)
avg_FPrich

# Alternatively, if I want standarzed coefficients 
model.avg(best_FPrich, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_FPrich)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_FPrich)

##################
# Best model     #
# Y=FP rich      #
# X=cond, secchi #
##################
formula <- FP_species_richness ~ COND_avg + secchi_avg
glm_poisson_FPrich_best <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_best)

# residuals 
glm_poisson_FPrich_best_resid <- resid(glm_poisson_FPrich_best)
glm_poisson_FPrich_best_resid <- as.vector(glm_poisson_FPrich_best_resid)
glm_poisson_FPrich_best_resid
# I may need to re-format this when I attach it to some other data frame 

#################
# Plot          #  
# X = Variable  #
# Y = Predicted #
#################