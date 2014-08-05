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
# All pairwise interactions          #
# excluded: depth_max_m, ALK_avg     #
######################################
formula <- FP_species_richness ~ surfacearea_ha + shoreline_development + nonFP_species_richness + TOTP_avg + PH_avg + COND_avg + secchi_avg +
  surfacearea_ha:shoreline_development                 +                                           
  surfacearea_ha:nonFP_species_richness                +                                           
  shoreline_development:nonFP_species_richness         +                                           
  surfacearea_ha:TOTP_avg                              +                                           
  shoreline_development:TOTP_avg                       +                                           
  nonFP_species_richness:TOTP_avg                      +                                           
  surfacearea_ha:PH_avg                                +                                           
  shoreline_development:PH_avg                         +                                           
  nonFP_species_richness:PH_avg                        +                                           
  TOTP_avg:PH_avg                                      +                                           
  surfacearea_ha:COND_avg                              +                                           
  shoreline_development:COND_avg                       +                                           
  nonFP_species_richness:COND_avg                      +                                           
  TOTP_avg:COND_avg                                    +                                           
  PH_avg:COND_avg                                      +                                           
  surfacearea_ha:secchi_avg                            +                                           
  shoreline_development:secchi_avg                     +                                           
  nonFP_species_richness:secchi_avg                    +                                           
  TOTP_avg:secchi_avg                                  +                                           
  PH_avg:secchi_avg                                    +                                           
  COND_avg:secchi_avg                                  
glm_poisson_FPrich_full_interactions <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_full_interactions)

##############################
# Full model w/ interactions #
# All sub-models             #
# Package: MuMIn             #
##############################
require(MuMIn)

# run all possible sub-models 
best_FPrich_interactions <- dredge(glm_poisson_FPrich_full_interactions)
best_FPrich_interactions

# write the table of all sub-models to file 
numb_models <- nrow(best_FPrich_interactions)
write.csv(best_FPrich_interactions[1:numb_models],file="best_FPrich_interactions.csv",na="NA")

# Return the best model (lowest AIC)
get.models(best_FPrich_interactions, 1)

# Visualize the model selection table:
plot(best_FPrich_interactions)

# Model average models with delta AICc < 2
avg_FPrich_interactions <- model.avg(best_FPrich_interactions, subset = delta < 2)
summary(avg_FPrich_interactions)
avg_FPrich_interactions

# Alternatively, if I want standarzed coefficients 
model.avg(best_FPrich_interactions, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_FPrich_interactions)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_FPrich_interactions)

######################################
# Define a full model                #
# No interactions                    #
# excluded: depth_max_m, ALK_avg     #
######################################
formula <- FP_species_richness ~ surfacearea_ha + shoreline_development + nonFP_species_richness + TOTP_avg + PH_avg + COND_avg + secchi_avg 
glm_poisson_FPrich_full <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_full)

###############################
# Full model w/o interactions #
# All sub-models              #
# Package: MuMIn              #
###############################
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

# Check the significance of the residual deviance 
glm_poisson_FPrich_best$df.resid # residual degrees of freedom
glm_poisson_FPrich_best$deviance # deviance
1 - pchisq(185.2,171) # entered manually: p = 0.2167009
1 - pchisq(glm_poisson_FPrich_best$deviance,glm_poisson_FPrich_best$df.resid) # called by name: p = 0.2167009

# Check the overdispersion (should be close to 1)
glm_poisson_FPrich_best$deviance/glm_poisson_FPrich_best$df.resid

# residuals 
glm_poisson_FPrich_best_resid <- resid(glm_poisson_FPrich_best)
glm_poisson_FPrich_best_resid <- as.vector(glm_poisson_FPrich_best_resid)
glm_poisson_FPrich_best_resid

# Output like an ANOVA table
anova(glm_poisson_FPrich_best, test="Chisq")



######################################
# Define a full model                #
# log of predictor variables 
# No interactions                    #
# excluded: depth_max_m, ALK_avg     #
######################################
formula <- FP_species_richness ~ log(surfacearea_ha) + log(shoreline_development) +
                                 nonFP_species_richness + log(TOTP_avg) + 
                                 log(PH_avg) + log(COND_avg) + log(secchi_avg) 
glm_poisson_FPrich_full_log <- glm(formula, family=poisson, data=data, na.action = "na.fail")
summary(glm_poisson_FPrich_full_log)


###############################
# Full model w/o interactions #
# log of predictor variables  #
# All sub-models              #
# Package: MuMIn              #
###############################
require(MuMIn)

# run all possible sub-models 
best_FPrich_log <- dredge(glm_poisson_FPrich_full_log)
best_FPrich_log

# write the table of all sub-models to file 
numb_models <- nrow(best_FPrich_log)
write.csv(best_FPrich_log[1:numb_models],file="best_FPrich_log.csv",na="NA")

# Return the best model (lowest AIC)
get.models(best_FPrich_log, 1)

# Visualize the model selection table:
plot(best_FPrich_log)

# Model average models with delta AICc < 2
avg_FPrich_log <- model.avg(best_FPrich_log, subset = delta < 2)
summary(avg_FPrich_log)
avg_FPrich_log

# Alternatively, if I want standarzed coefficients 
model.avg(best_FPrich, subset = delta < 2, beta = TRUE)

# returns coefficients 
coef(avg_FPrich_log)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_FPrich_log)
