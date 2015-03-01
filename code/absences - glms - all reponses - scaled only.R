########################
# GLMs                 #
# Y = species presence #
########################
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - data imported.RData")

###############
# Lemna minor #
# Full & Null #
###############
# Build full and null models

colnames(dataENV_scaled)
temp_data_LM <- dataENV_scaled
temp_data_LM$nearest_SP <- NULL  # remove nearest SP
temp_data_LM$nearest_W <- NULL   # remove nearest W
temp_data_LM$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_LM$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make the formula 
formula_LM <- as.formula(paste("LM ~ ", paste(colnames(temp_data_LM), collapse= "+")))
temp_data_LM$LM <- data$lemna_minor # add LM presence 
formula_LM # check out the formula 

# GLM for full model 
glm_LM_scaled <- glm(formula_LM, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_scaled)

# null model 
glm_LM_scaled_null <- glm(LM ~ 1, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_scaled_null)

#######################
# Spirodela polyrhiza #
# Full & Null         #
#######################
# Build full and null models

colnames(dataENV_scaled)
temp_data_SP <- dataENV_scaled
temp_data_SP$nearest_LM <- NULL  # remove nearest LM
temp_data_SP$nearest_W <- NULL   # remove nearest W
temp_data_SP$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_SP$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_SP <- as.formula(paste("SP ~ ", paste(colnames(temp_data_SP), collapse= "+")))
temp_data_SP$SP <- data$spirodela_polyrhiza # add SP presence 
formula_SP # check out the formula 

# GLM for full model 
glm_SP_scaled <- glm(formula_SP, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_scaled)

# null model 
glm_SP_scaled_null <- glm(SP ~ 1, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_scaled_null)

###############
# Wolffia sp. #
# Full & Null #
###############
# Build full and null models

colnames(dataENV_scaled)
temp_data_W <- dataENV_scaled
temp_data_W$nearest_LM <- NULL  # remove nearest LM
temp_data_W$nearest_SP <- NULL   # remove nearest SP
temp_data_W$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_W$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_W <- as.formula(paste("W ~ ", paste(colnames(temp_data_W), collapse= "+")))
temp_data_W$W <- data$wolffia_sp # add W presence 
formula_W # view the formula 

# GLM for full model 
glm_W_scaled <- glm(formula_W, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_scaled)

# null model 
glm_W_scaled_null <- glm(W ~ 1, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_scaled_null)

###############
# FP presence # 
# Full & Null #
###############
# Build full and null models

colnames(dataENV_scaled)
temp_data_FPpres <- dataENV_scaled
temp_data_FPpres$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPpres)

# make formula 
formula_FPpres <- as.formula(paste("FPpres ~ ", paste(colnames(temp_data_FPpres), collapse= "+")))
temp_data_FPpres$FPpres <- as.numeric(data$FP_presence) # add FP ppresence 
formula_FPpres # view the formula 

# GLM for full model 
glm_FPpres_scaled <- glm(formula_FPpres, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_scaled)

# null model 
glm_FPpres_scaled_null <- glm(FPpres ~ 1, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_scaled_null)

###############
# FP richness # 
# Full & Null #
###############
# Build full and null models

colnames(dataENV_scaled)
temp_data_FPrich <- dataENV_scaled
temp_data_FPrich$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPrich)

# make formula 
formula_FPrich <- as.formula(paste("FPrich ~ ", paste(colnames(temp_data_FPrich), collapse= "+")))
temp_data_FPrich$FPrich <- data$FP_species_richness # add FP ppresence 
formula_FPrich # view the formula 

# GLM for full model 
glm_FPrich_scaled <- glm(formula_FPrich, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_scaled)

# null model 
glm_FPrich_scaled_null <- glm(FPrich ~ 1, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_scaled_null)

############################
# AICs for all full models #
############################
AIC(glm_LM_scaled)
AIC(glm_SP_scaled) 
AIC(glm_W_scaled) 
AIC(glm_FPpres_scaled)
AIC(glm_FPrich_scaled)

############################
# AICs for all null models #
############################
AIC(glm_LM_scaled_null)
AIC(glm_SP_scaled_null) 
AIC(glm_W_scaled_null) 
AIC(glm_FPpres_scaled_null)
AIC(glm_FPrich_scaled_null)

#########################
# Run all nested models #
#########################
library(MuMIn)

# this may take awhile 
all_glm_LM_scaled <- dredge(glm_LM_scaled)
numb_models <- nrow(all_glm_LM_scaled)
write.csv(all_glm_LM_scaled[1:numb_models],file="all_glm_LM_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

all_glm_SP_scaled <- dredge(glm_SP_scaled)
numb_models <- nrow(all_glm_SP_scaled)
write.csv(all_glm_SP_scaled[1:numb_models],file="all_glm_SP_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

all_glm_W_scaled <- dredge(glm_W_scaled)
numb_models <- nrow(all_glm_W_scaled)
write.csv(all_glm_W_scaled[1:numb_models],file="all_glm_W_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

all_glm_FPpres_scaled <- dredge(glm_FPpres_scaled)
numb_models <- nrow(all_glm_FPpres_scaled)
write.csv(all_glm_FPpres_scaled[1:numb_models],file="all_glm_FPpres_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

all_glm_FPrich_scaled <- dredge(glm_FPrich_scaled) # do this one later 
numb_models <- nrow(all_glm_FPrich_scaled) # do this one later 
write.csv(all_glm_FPrich_scaled[1:numb_models],file="all_glm_FPrich_scaled_2.csv",na="NA")

##################################
# Running all models in parallel #
# pdredge()                      #
##################################
# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_LM")

# run the model in parallel
all_glm_LM_scaled <- pdredge(glm_LM_scaled, cluster=clust)
numb_models <- nrow(all_glm_LM_scaled)
write.csv(all_glm_LM_scaled[1:numb_models],file="all_glm_LM_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_SP")

# run the model in parallel
all_glm_SP_scaled <- pdredge(glm_SP_scaled, cluster=clust)
numb_models <- nrow(all_glm_SP_scaled)
write.csv(all_glm_SP_scaled[1:numb_models],file="all_glm_SP_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

# stop the cluster 
stopCluster(clust)



# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_W")

# run the model in parallel
all_glm_W_scaled <- pdredge(glm_W_scaled, cluster=clust)
numb_models <- nrow(all_glm_W_scaled)
write.csv(all_glm_W_scaled[1:numb_models],file="all_glm_W_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPpres")

# run the model in parallel
all_glm_FPpres_scaled <- pdredge(glm_FPpres_scaled, cluster=clust)
numb_models <- nrow(all_glm_FPpres_scaled)
write.csv(all_glm_FPpres_scaled[1:numb_models],file="all_glm_FPpres_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPrich")

# run the model in parallel
all_glm_FPrich_scaled <- pdredge(glm_FPrich_scaled, cluster=clust)
numb_models <- nrow(all_glm_FPrich_scaled)
write.csv(all_glm_FPrich_scaled[1:numb_models],file="all_glm_FPrich_scaled_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled.RData")

# stop the cluster 
stopCluster(clust)



write.csv(all_glm_LM_scaled[1:numb_models],file="all_glm_LM_scaled.csv",na="NA")
numb_models <- nrow(all_glm_SP_scaled)
write.csv(all_glm_SP_scaled[1:numb_models],file="all_glm_SP_scaled.csv",na="NA")
numb_models <- nrow(all_glm_W_scaled)
write.csv(all_glm_W_scaled[1:numb_models],file="all_glm_W_scaled.csv",na="NA")
numb_models <- nrow(all_glm_FPpres_scaled)
write.csv(all_glm_FPpres_scaled[1:numb_models],file="all_glm_FPpres_scaled_2.csv",na="NA")
numb_models <- nrow(all_glm_FPrich_scaled)
write.csv(all_glm_FPrich_scaled[1:numb_models],file="all_glm_FPrich_scaled_2.csv",na="NA")
write.csv(all_glm_FPpres_scaled[1:numb_models],file="all_glm_FPpres_scaled.csv",na="NA")

write.csv(all_glm_LM_scaled[1:numb_models],file="all_glm_LM_scaled.csv",na="NA")
numb_models <- nrow(all_glm_SP_scaled)
write.csv(all_glm_SP_scaled[1:numb_models],file="all_glm_SP_scaled.csv",na="NA")
numb_models <- nrow(all_glm_W_scaled)
write.csv(all_glm_W_scaled[1:numb_models],file="all_glm_W_scaled.csv",na="NA")
numb_models <- nrow(all_glm_FPpres_scaled)
write.csv(all_glm_FPpres_scaled[1:numb_models],file="all_glm_FPpres_scaled.csv",na="NA")

##############
# Best model #  
##############
library(MuMIn)

# assign the best model to an object 
best_glm_LM_scaled <- get.models(all_glm_LM_scaled, 1)[1]
best_glm_SP_scaled <- get.models(all_glm_SP_scaled, 1)[1]
best_glm_W_scaled <- get.models(all_glm_W_scaled, 1)[1]
best_glm_FPpres_scaled <- get.models(all_glm_FPpres_scaled, 1)[1]

# but we actually want this to be the fitted model 
# I deleted a "+ 1" from the end of each of these formulas - not sure why it was there (MJM 8/27/2014) 
best_glm_LM_scaled <- glm(LM ~ COND_avg + depth_max_m + dist_waterfowl + latitude + TOTP_avg, 
                         family = binomial, 
                         data = temp_data_LM, 
                         na.action = "na.fail")

best_glm_SP_scaled <- glm(SP ~ ALK_avg + secchi_avg, 
                         family = binomial, 
                         data = temp_data_SP, 
                         na.action = "na.fail")

best_glm_W_scaled <- glm(W ~ boatlaunch + COND_avg + depth_max_m + latitude + waterbodies_10km, 
                        family = binomial, 
                        data = temp_data_W, na.action = "na.fail")

best_glm_FPpres_scaled <- glm(FPpres ~ COND_avg + latitude + nearest_LM + nearest_SP + secchi_avg + TOTP_avg,
                             family = binomial, 
                             data = temp_data_FPpres, 
                             na.action = "na.fail")

best_glm_FPrich_scaled <- glm(FPrich ~ ALK_avg + boatlaunch + COND_avg + depth_max_m + dist_waterfowl + latitude + nearest_LM + TOTP_avg,
                             family = poisson, 
                             data = temp_data_FPrich, 
                             na.action = "na.fail")

summary(best_glm_LM_scaled)
summary(best_glm_SP_scaled) 
summary(best_glm_W_scaled) 
summary(best_glm_FPpres_scaled)
summary(best_glm_FPrich_scaled)

AIC(best_glm_LM_scaled)
AIC(best_glm_SP_scaled) 
AIC(best_glm_W_scaled) 
AIC(best_glm_FPpres_scaled)
AIC(best_glm_FPrich_scaled)

###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM_scaled <- model.avg(all_glm_LM_scaled, subset = delta < 2)
avg_SP_scaled <- model.avg(all_glm_SP_scaled, subset = delta < 2)
avg_W_scaled <- model.avg(all_glm_W_scaled, subset = delta < 2)
avg_FPpres_scaled <- model.avg(all_glm_FPpres_scaled, subset = delta < 2)

# Re-do the model averaging based on explicity specifying a model list 
# this is so I can use predict() with the model average object 
model_list_LM <- get.models(all_glm_LM_scaled, subset = delta < 2) # specify the list of models whose delta AIC < 2
model_list_SP <- get.models(all_glm_SP_scaled, subset = delta < 2)
model_list_W <- get.models(all_glm_W_scaled, subset = delta < 2)
model_list_FPpress <- get.models(all_glm_FPpres_scaled, subset = delta < 2)
model_list_FPrich <- get.models(all_glm_FPrich_scaled, subset = delta < 2)

avg_LM_scaled <- model.avg(model_list_LM) # use that list to do the model averaging 
avg_SP_scaled <- model.avg(model_list_SP)
avg_W_scaled <- model.avg(model_list_W)
avg_FPpres_scaled <- model.avg(model_list_FPpress)
avg_FPrich_scaled <- model.avg(model_list_FPrich)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM_scaled)
summary(avg_SP_scaled)
summary(avg_W_scaled)
summary(avg_FPpres_scaled)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM_scaled)
importance(avg_SP_scaled)
importance(avg_W_scaled)
importance(avg_FPpres_scaled)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM_scaled)
confint(avg_SP_scaled)
confint(avg_W_scaled)
confint(avg_FPpres_scaled)

######################
# pseudo-R-squared   #
# Variance explained #
# Best model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(best_glm_LM_scaled, null = glm_LM_scaled_null)
r.squaredLR(best_glm_SP_scaled, null = glm_SP_scaled_null)
r.squaredLR(best_glm_W_scaled, null = glm_W_scaled_null)
r.squaredLR(best_glm_FPpres_scaled, null = glm_FPpres_scaled_null)

######################
# pseudo-R-squared   #
# Variance explained #
# Ful model          #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(glm_LM_scaled, null = glm_LM_scaled_null)
r.squaredLR(glm_SP_scaled, null = glm_SP_scaled_null)
r.squaredLR(glm_W_scaled, null = glm_W_scaled_null)
r.squaredLR(glm_FPpres_scaled, null = glm_FPpres_scaled_null)

####################### 
# Signif. of deviance #
# Full model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(glm_LM_scaled) # Residual Deviance
resid_df <- as.numeric(summary(glm_LM_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_SP_scaled) # Residual Deviance
resid_df <- as.numeric(summary(glm_SP_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_W_scaled) # Residual Deviance
resid_df <- as.numeric(summary(glm_W_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_FPpres_scaled) # Residual Deviance
resid_df <- as.numeric(summary(glm_FPpres_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

####################### 
# Signif. of deviance #
# Best model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(best_glm_LM_scaled) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_LM_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_SP_scaled) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_SP_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_W_scaled) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_W_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_FPpres_scaled) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_FPpres_scaled)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

######################
# Deviance explained #
# Full model         #
######################
null_dev <- as.numeric(summary(glm_LM_scaled)[8]) # Null Deviance      
resid_dev <- deviance(glm_LM_scaled) # Residual Deviance
glm_LM_scaled_devExpl <- (null_dev - resid_dev) / null_dev
glm_LM_scaled_devExpl

null_dev <- as.numeric(summary(glm_SP_scaled)[8]) # Null Deviance:      
resid_dev <- deviance(glm_SP_scaled) # Residual Deviance: 
glm_SP_scaled_devExpl <- (null_dev - resid_dev) / null_dev
glm_SP_scaled_devExpl

null_dev <- as.numeric(summary(glm_W_scaled)[8]) # Null Deviance: 
resid_dev <- deviance(glm_W_scaled) # Residual Deviance: 
glm_W_scaled_devExpl <- (null_dev - resid_dev) / null_dev
glm_W_scaled_devExpl

null_dev <- as.numeric(summary(glm_FPpres_scaled)[8]) # Null Deviance: 
resid_dev <- deviance(glm_FPpres_scaled) # Residual Deviance: 
glm_FPpres_scaled_devExpl <- (null_dev - resid_dev) / null_dev
glm_FPpres_scaled_devExpl

######################
# Deviance explained #
# Best model         #
######################
null_dev <- as.numeric(summary(best_glm_LM_scaled)[8]) # Null Deviance      
resid_dev <- deviance(best_glm_LM_scaled) # Residual Deviance
best_glm_LM_scaled_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_LM_scaled_devExpl

null_dev <- as.numeric(summary(best_glm_SP_scaled)[8]) # Null Deviance:      
resid_dev <- deviance(best_glm_SP_scaled) # Residual Deviance: 
best_glm_SP_scaled_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_SP_scaled_devExpl

null_dev <- as.numeric(summary(best_glm_W_scaled)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_W_scaled) # Residual Deviance: 
best_glm_W_scaled_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_W_scaled_devExpl

null_dev <- as.numeric(summary(best_glm_FPpres_scaled)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_FPpres_scaled) # Residual Deviance: 
best_glm_FPpres_scaled_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_FPpres_scaled_devExpl

#################### 
# Cross-validation #
# Best model       #
####################
# 10-fold cross validation of best model 
library(boot)

# Detlta1: raw CV est. of prediction error. 
# Delta2: adjusted CV estimate to compensate for bias introduced by not using leave-one-out cross-validation.

#  Since the response is a binary variable an appropriate cost function is
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

best_glm_LM_scaled_CV <- cv.glm(data=temp_data_LM, best_glm_LM_scaled, cost, K=10)
best_glm_LM_scaled_CV$delta

best_glm_SP_scaled_CV <- cv.glm(data=temp_data_SP, best_glm_SP_scaled, cost, K=10)
best_glm_SP_scaled_CV$delta

best_glm_W_scaled_CV <- cv.glm(data=temp_data_W, best_glm_W_scaled, cost, K=10)
best_glm_W_scaled_CV$delta

best_glm_FPpres_scaled_CV <- cv.glm(data=temp_data_FPpres, best_glm_FPpres_scaled, cost, K=10)
best_glm_FPpres_scaled_CV$delta

#################### 
# Cross-validation #
# Full model       #
####################
# 10-fold cross validation of best model 
library(boot)

# Detlta1: raw CV est. of prediction error. 
# Delta2: adjusted CV estimate to compensate for bias introduced by not using leave-one-out cross-validation.

#  Since the response is a binary variable an appropriate cost function is
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

full_glm_LM_scaled_CV <- cv.glm(data=temp_data_LM, glm_LM_scaled, cost, K=10)
full_glm_LM_scaled_CV$delta

full_glm_SP_scaled_CV <- cv.glm(data=temp_data_SP, glm_SP_scaled, cost, K=10)
full_glm_SP_scaled_CV$delta

full_glm_W_scaled_CV <- cv.glm(data=temp_data_W, glm_W_scaled, cost, K=10)
full_glm_W_scaled_CV$delta

full_glm_FPpres_scaled_CV <- cv.glm(data=temp_data_FPpres, glm_FPpres_scaled, cost, K=10)
full_glm_FPpres_scaled_CV$delta

################# 
# Outlier Tests #
# Best model    #
#################
library(car)
outlierTest(best_glm_LM_scaled)
outlierTest(best_glm_SP_scaled)
outlierTest(best_glm_W_scaled)
outlierTest(best_glm_FPpres_scaled)

################# 
# Outlier Tests #
# Full model    #
#################
library(car)
outlierTest(glm_LM_scaled)
outlierTest(glm_SP_scaled)
outlierTest(glm_W_scaled)
outlierTest(glm_FPpres_scaled)

###################
# Residuals plots #
# Best model      #
###################
# If plots is curved - the model fit does not accurately describe the data 
library(car)
residualPlots(best_glm_LM_scaled)
residualPlots(best_glm_SP_scaled)
residualPlots(best_glm_W_scaled)
residualPlots(best_glm_FPpres_scaled)

# returns lack-of-fit test is computed for each numeric predictor

jpeg("best_glm_LM_scaled_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_LM_scaled,main="Lemna minor - best model glm - residuals plot")
dev.off()

jpeg("best_glm_SP_scaled_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_SP_scaled,main="Spirodela polyrhiza - best model glm - residuals plot")
dev.off()

jpeg("best_glm_W_scaled_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_W_scaled,main="Wolffia sp. - best model glm -  residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_scaled_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_FPpres_scaled,main="FP presence - best model glm -  residuals plot",smoother=F)
dev.off()

##########################
# Partial residuals plot #
# Best model             # 
##########################
# effect of predictors given other predictors are also in the model (i.e., controlling for those predictors)
library(car)
crPlots(best_glm_LM_scaled)
crPlots(best_glm_SP_scaled)
crPlots(best_glm_W_scaled)
crPlots(best_glm_FPpres_scaled)

jpeg("best_glm_LM_scaled_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_LM_scaled,main="Lemna minor - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_SP_scaled_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_SP_scaled,main="Spirodela polyrhiza - best model glm - partial residuals plot",smoother=F,layout=1)
dev.off()

jpeg("best_glm_W_scaled_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_W_scaled,main="Wolffia sp. - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_scaled_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_FPpres_scaled,main="FP presence - best model glm - partial residuals plot",smoother=F)
dev.off()

# or 

library(visreg)
visreg(best_glm_LM_scaled)
visreg(best_glm_SP_scaled)
visreg(best_glm_W_scaled)
visreg(best_glm_FPpres_scaled)

##############################
# Model-average effect sizes #
##############################
# Sequeira et al. 2014 

# for each predictor 
# 1. divide coefficient estimate by standard error 
# 2. calculate fraction of wAIC from sum of wAIC where predictor occurs 
# 3. multiple (1) by (2)
# 4. sum up (3) for all models where they occur 

###################
# Model residuals # 
# Best model      #
################### 
# get model residuals from the single best model 

best_glm_LM_scaled_resid <- resid(best_glm_LM_scaled)
best_glm_LM_scaled_resid <- as.vector(best_glm_LM_scaled_resid)

best_glm_SP_scaled_resid <- resid(best_glm_SP_scaled)
best_glm_SP_scaled_resid <- as.vector(best_glm_SP_scaled_resid)

best_glm_W_scaled_resid <- resid(best_glm_W_scaled)
best_glm_W_scaled_resid <- as.vector(best_glm_W_scaled_resid)

best_glm_FPpres_scaled_resid <- resid(best_glm_FPpres_scaled)
best_glm_FPpres_scaled_resid <- as.vector(best_glm_FPpres_scaled_resid)


####################################
# Spatial correlation of residuals #
####################################
library(spdep)
sp.correlogram()

# need a matrix of distances between all pairs 
# I used a different package and functions last time - compare to this 


####################
# Confusion matrix #
# Best model       #
####################
# define the function to calculate a confusion matrix 
# https://gist.github.com/ryanwitt/2911560
# this only works for logistic 
confusion.glm <- function(data, model) {
  # this gives you the prediction for each observation 
  prediction <- ifelse(predict(model, data, type='response') > 0.5, "present", "absent")
  # this makes the table of the predictions vs. the observed  
  confusion  <- table(prediction, ifelse(model$y==1, "present", "absent"))
  # I'm not sure what this classification error is so I'm going to shut it off 
  # confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  # confusion  <- as.data.frame(confusion)
  names(confusion) <- c('absent', 'present')
  #names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

# try with real data 
prediction <- ifelse(predict(best_glm_LM_scaled, temp_data_LM, type='response') > 0.5, "present", "absent")
# this makes the table of the predictions vs. the observed  
confusion  <- table(prediction, ifelse(best_glm_LM_scaled$y==1, "present", "absent"))
# I'm not sure what this classification error is so I'm going to shut it off 
# confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
# confusion  <- as.data.frame(confusion)
names(confusion) <- c('absent', 'present')
#names(confusion) <- c('FALSE', 'TRUE', 'class.error')
confusion

confusion.glm(temp_data_LM,best_glm_LM_scaled)
confusion.glm(temp_data_SP,best_glm_SP_scaled) # the model only predicts absences 
confusion.glm(temp_data_W,best_glm_W_scaled)
confusion.glm(temp_data_FPpres,best_glm_FPpres_scaled)

# cannot do it for FP richness - only works for logistic regression 
confusion.glm(temp_data_FPrich,best_glm_FPrich_scaled)

####################
# Confusion matrix #
# Model average    #
####################
# I cannot use the function that I defined above 
# a model.avg() object does not keep the observed values like a fitted.glm.object$y does 

# LM #
prediction <- ifelse(predict(avg_LM_scaled, temp_data_LM, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_LM_scaled$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# SP # 
prediction <- ifelse(predict(avg_SP_scaled, temp_data_SP, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_SP_scaled$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# W # 
prediction <- ifelse(predict(avg_W_scaled, temp_data_W, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_W_scaled$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# FPpres # 
prediction <- ifelse(predict(avg_FPpres_scaled, temp_data_FPpres, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_FPpres_scaled$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

###############################
# Generating predicted values #
###############################
predict(best_glm_LM_scaled, temp_data_LM, type="response")
predict(best_glm_SP_scaled, temp_data_SP, type="response")
predict(best_glm_W_scaled, temp_data_W, type="response")
predict(best_glm_FPrich_scaled, temp_data_FPrich, type="response")
predict(best_glm_FPpres_scaled, temp_data_FPpres, type="response")

############
# clean up #   
############
#rm(null_dev,resid_dev,temp_data)