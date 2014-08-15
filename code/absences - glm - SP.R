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
formula <- spirodela_polyrhiza ~ log(latitude) + log(longitude) + 
  log(surfacearea_ha) + log(shoreline_development) +
  log(nonFP_species_richness+1) + log(TOTP_avg) + 
  log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
  log(waterbodies_1km+1) + log(waterbodies_10km) + 
  boatlaunch + log(dist_waterfowl+1) + 
  log(nearest_SP) #+ major_watershed + regional_watershed

glm_binomial_SP_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")

summary(glm_binomial_SP_full_log)

# null model 
glm_binomial_SP_null <- glm(spirodela_polyrhiza ~ 1, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_SP_null)


##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_SP__log <- dredge(glm_binomial_SP_full_log)
best_SP__log

# write the table of all sub-models to file 
numb_models <- nrow(best_SP__log)
write.csv(best_SP__log[1:numb_models],file="best_glm_SP_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_SP__log, 1)

# proportion deviance explained 
# Null Deviance:      163.1 
# Residual Deviance: 158.5 
d2 <- (163.1 - 158.5)/163.1

# adjusted proportion deviance explained 
# http://modtools.wordpress.com/2013/08/14/dsquared/
# d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
# where n = # of observations and p = # of parameters (including the intercept)
n <- 174
p <- 2
d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
d2_adj

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

