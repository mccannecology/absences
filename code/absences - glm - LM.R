##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
#                                            #
# Presence / Absence of Lemna minor          #
# Generalized linear model                   #
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


######################
# Full model         #
# no transformations #
######################
formula <- lemna_minor ~ latitude + longitude + 
  surfacearea_ha + shoreline_development +
  nonFP_species_richness + TOTP_avg + 
  PH_avg + COND_avg + secchi_avg + 
  waterbodies_1km + waterbodies_10km + 
  boatlaunch + dist_waterfowl + nearest_LM 

glm_LM_full <- glm(formula, family=binomial, data=data, na.action = "na.fail")

summary(glm_LM_full)

##############
# Sub-models #
##############
# nearest_LM is (-)
formula <- lemna_minor ~ nearest_LM 
glm_LM_sub01 <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_LM_sub01)

# nearest_LM is (-)
formula <- lemna_minor ~ nearest_LM + COND_avg
glm_LM_sub02 <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_LM_sub02)

# nearest_LM is (+)
formula <- lemna_minor ~ nearest_LM + COND_avg + secchi_avg
glm_LM_sub03 <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_LM_sub03)

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
formula <- lemna_minor ~ log(latitude) + log(longitude) + 
  log(surfacearea_ha) + log(shoreline_development) +
  log(nonFP_species_richness+1) + log(TOTP_avg) + 
  log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
  log(waterbodies_1km+1) + log(waterbodies_10km) + 
  boatlaunch + log(dist_waterfowl+1) + log(nearest_LM) #+ 
  #major_watershed + regional_watershed

glm_binomial_LM_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")

summary(glm_binomial_LM_full_log)

# proportion deviance explained 
# Null Deviance:      218.6 
# Residual Deviance: 169.5 
d2 <- (218.6 - 166.3)/218.6
d2

# null model 
glm_binomial_LM_null <- glm(lemna_minor ~ 1, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_LM_null)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_LM_log <- dredge(glm_binomial_LM_full_log)
best_LM_log

# write the table of all sub-models to file 
numb_models <- nrow(best_LM_log)
write.csv(best_LM_log[1:numb_models],file="best_glm_LM_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_LM_log, 1)

# proportion deviance explained 
# Null Deviance:      218.6 
# Residual Deviance: 169.5 
d2 <- (218.6 - 169.5)/218.6
d2

# adjusted proportion deviance explained 
# http://modtools.wordpress.com/2013/08/14/dsquared/
# d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
# where n = # of observations and p = # of parameters (including the intercept)
n <- 174
p <- 6
d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
d2_adj

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