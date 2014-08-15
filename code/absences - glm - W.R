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
formula <- wolffia_sp ~ log(latitude) + log(longitude) + 
  log(surfacearea_ha) + log(shoreline_development) +
  log(nonFP_species_richness+1) + log(TOTP_avg) + 
  log(PH_avg) + log(COND_avg) + log(secchi_avg) + 
  log(waterbodies_1km+1) + log(waterbodies_10km) + 
  boatlaunch + log(dist_waterfowl+1) + 
  log(nearest_W) #+ major_watershed + regional_watershed

glm_binomial_W_full_log <- glm(formula, family=binomial, data=data, na.action = "na.fail")

summary(glm_binomial_W_full_log)

# null model 
glm_binomial_W_null <- glm(wolffia_sp ~ 1, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_W_null)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_W_log <- dredge(glm_binomial_W_full_log)
best_W_log

# write the table of all sub-models to file 
numb_models <- nrow(best_W_log)
write.csv(best_W_log[1:numb_models],file="best_glm_W_log.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_W_log, 1)

# proportion deviance explained 
# Null Deviance:      135.9 
# Residual Deviance: 116.7 
d2 <- (135.9 - 116.7)/135.9

# adjusted proportion deviance explained 
# http://modtools.wordpress.com/2013/08/14/dsquared/
# d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
# where n = # of observations and p = # of parameters (including the intercept)
n <- 174
p <- 4
d2_adj <- 1 - ((n - 1) / (n - p)) * (1 - d2)
d2_adj

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
