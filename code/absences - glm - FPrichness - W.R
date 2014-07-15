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
data$wolffia_sp <- data$wolffia + data$wolffia_borealis + data$wolffia_brasiliensis 


######################################
# Define a full model                #
# not included: depth_max_m, ALK_avg #
# No interactions                    #
######################################
formula <- wolffia_sp ~ surfacearea_ha + shoreline_development + nonFP_species_richness+ TOTP_avg + PH_avg + COND_avg + secchi_avg
glm_binomial_W_full <- glm(formula, family=binomial, data=data, na.action = "na.fail")
summary(glm_binomial_W_full)

##################
# All sub-models #
# Package: MuMIn #
##################
require(MuMIn)
best_W <- dredge(glm_binomial_W_full)
numb_models <- nrow(best_W)
write.csv(best_W[1:numb_models],file="best_W.csv",na="NA")

#'Best' model (lowest AIC)
get.models(best_W, 1)

# print all model outputs
get.models(best_W)

# Visualize the model selection table:
plot(best_W)

# Model average models with delta AICc < 4
avg_W <- model.avg(best_W, subset = delta < 4)
summary(avg_W)
avg_W

# or as a 95% confidence set:
avg_W_95p <- model.avg(best_W, subset = cumsum(weight) <= .95) 
confint(avg_W_95p)

# returns standarzed coefficients 
model.avg(best_W, subset = delta < 4, beta = TRUE)

# returns coefficients 
coef(avg_W)

# can only predict from 'averaging' object created with a model list
predict(avg_W)

# Relative importance values 
# Sum of ‘Akaike weights’ over all models including the explanatory variable
# These don't sum up to one (across all variables)
importance(best_W)


#################
# Plot          #  
# X = Variable  #
# Y = Predicted #
#################