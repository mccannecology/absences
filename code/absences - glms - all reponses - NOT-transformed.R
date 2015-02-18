########################
# GLMs                 #
# Y = species presence #
# 8/20/2014            #
########################

# Select the predictors that I need from "data" data frame 
data_glm_raw <- data[,c("surfacearea_ha","shoreline_development","depth_max_m",
                        "TOTP_avg","COND_avg","ALK_avg","secchi_avg",
                        "waterbodies_1km","waterbodies_10km","dist_waterfowl",
                        "nearest_LM","nearest_SP","nearest_W","latitude",
                        "longitude","boatlaunch","nonFP_species_richness")]

###############
# Lemna minor #
# Full & Null #
###############
# Build full and null models

colnames(data_glm_raw)
temp_data_LM <- data_glm_raw
temp_data_LM$nearest_SP <- NULL  # remove nearest SP
temp_data_LM$nearest_W <- NULL   # remove nearest W
temp_data_LM$nearest_any_FP <- NULL   # remove nearest LM,SP,or W

# make the formula 
formula_LM <- as.formula(paste("LM ~ ", paste(colnames(temp_data_LM), collapse= "+")))
temp_data_LM$LM <- data$lemna_minor # add LM presence 
formula_LM # check out the formula 

# GLM for full model 
glm_LM <- glm(formula_LM, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM)

# null model 
glm_LM_null <- glm(LM ~ 1, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_null)

#######################
# Spirodela polyrhiza #
# Full & Null         #
#######################
# Build full and null models

colnames(data_glm_raw)
temp_data_SP <- data_glm_raw
temp_data_SP$nearest_LM <- NULL  # remove nearest LM
temp_data_SP$nearest_W <- NULL   # remove nearest W
temp_data_SP$nearest_any_FP <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_SP <- as.formula(paste("SP ~ ", paste(colnames(temp_data_SP), collapse= "+")))
temp_data_SP$SP <- data$spirodela_polyrhiza # add SP presence 
formula_SP # check out the formula 

# GLM for full model 
glm_SP <- glm(formula_SP, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP)

# null model 
glm_SP_null <- glm(SP ~ 1, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_null)

###############
# Wolffia sp. #
# Full & Null #
###############
# Build full and null models

colnames(data_glm_raw)
temp_data_W <- data_glm_raw
temp_data_W$nearest_LM <- NULL  # remove nearest LM
temp_data_W$nearest_SP <- NULL   # remove nearest SP
temp_data_W$nearest_any_FP <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_W <- as.formula(paste("W ~ ", paste(colnames(temp_data_W), collapse= "+")))
temp_data_W$W <- data$wolffia_sp # add W presence 
formula_W # view the formula 

# GLM for full model 
glm_W <- glm(formula_W, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W)

# null model 
glm_W_null <- glm(W ~ 1, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_null)

###############
# FP presence # 
# Full & Null #
###############
# Build full and null models

colnames(data_glm_raw)
temp_data_FPpres <- data_glm_raw
temp_data_FPpres$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPpres)

# make formula 
formula_FPpres <- as.formula(paste("FPpres ~ ", paste(colnames(temp_data_FPpres), collapse= "+")))
temp_data_FPpres$FPpres <- as.numeric(data$FP_presence) # add FP ppresence 
formula_FPpres # view the formula 

# GLM for full model 
glm_FPpres <- glm(formula_FPpres, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres)

# null model 
glm_FPpres_null <- glm(FPpres ~ 1, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_null)

###############
# FP richness # 
# Full & Null #
###############
# Build full and null models

colnames(data_glm_raw)
temp_data_FPrich <- data_glm_raw
temp_data_FPrich$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPrich)

# make formula 
formula_FPrich <- as.formula(paste("FPrich ~ ", paste(colnames(temp_data_FPrich), collapse= "+")))
temp_data_FPrich$FPrich <- data$FP_species_richness # add FP ppresence 
formula_FPrich # view the formula 

# GLM for full model 
glm_FPrich <- glm(formula_FPrich, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich)

# null model 
glm_FPrich_null <- glm(FPrich ~ 1, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_null)

############################
# AICs for all full models #
############################
AIC(glm_LM)
AIC(glm_SP) 
AIC(glm_W) 
AIC(glm_FPpres)
AIC(glm_FPrich)

############################
# AICs for all null models #
############################
AIC(glm_LM_null)
AIC(glm_SP_null) 
AIC(glm_W_null) 
AIC(glm_FPpres_null)
AIC(glm_FPrich_null)


#########################
# Run all nested models #
#########################
library(MuMIn)

# this may take awhile 
all_glm_LM <- dredge(glm_LM)
numb_models <- nrow(all_glm_LM)
write.csv(all_glm_LM[1:numb_models],file="all_glm_LM_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

all_glm_SP <- dredge(glm_SP)
numb_models <- nrow(all_glm_SP)
write.csv(all_glm_SP[1:numb_models],file="all_glm_SP_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

all_glm_W <- dredge(glm_W)
numb_models <- nrow(all_glm_W)
write.csv(all_glm_W[1:numb_models],file="all_glm_W_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

all_glm_FPpres <- dredge(glm_FPpres)
numb_models <- nrow(all_glm_FPpres)
write.csv(all_glm_FPpres[1:numb_models],file="all_glm_FPpres_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

all_glm_FPrich <- dredge(glm_FPrich) # do this one later 
numb_models <- nrow(all_glm_FPrich) # do this one later 
write.csv(all_glm_FPrich[1:numb_models],file="all_glm_FPrich_2.csv",na="NA")

##################################
# Running all models in parallel #
# pdredge()                      #
##################################
# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_LM")

# run the model in parallel
all_glm_LM <- pdredge(glm_LM, cluster=clust)
numb_models <- nrow(all_glm_LM)
write.csv(all_glm_LM[1:numb_models],file="all_glm_LM_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_SP")

# run the model in parallel
all_glm_SP <- pdredge(glm_SP, cluster=clust)
numb_models <- nrow(all_glm_SP)
write.csv(all_glm_SP[1:numb_models],file="all_glm_SP_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

# stop the cluster 
stopCluster(clust)



# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_W")

# run the model in parallel
all_glm_W <- pdredge(glm_W, cluster=clust)
numb_models <- nrow(all_glm_W)
write.csv(all_glm_W[1:numb_models],file="all_glm_W_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPpres")

# run the model in parallel
all_glm_FPpres <- pdredge(glm_FPpres, cluster=clust)
numb_models <- nrow(all_glm_FPpres)
write.csv(all_glm_FPpres[1:numb_models],file="all_glm_FPpres_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPrich")

# run the model in parallel
all_glm_FPrich <- pdredge(glm_FPrich, cluster=clust)
numb_models <- nrow(all_glm_FPrich)
write.csv(all_glm_FPrich[1:numb_models],file="all_glm_FPrich_2.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - raw.RData")

# stop the cluster 
stopCluster(clust)






##############
# Best model #  
##############
library(MuMIn)

# assign the best model to an object 
best_glm_LM <- get.models(all_glm_LM, 1)[1]
best_glm_SP <- get.models(all_glm_SP, 1)[1]
best_glm_W <- get.models(all_glm_W, 1)[1]
best_glm_FPpres <- get.models(all_glm_FPpres, 1)[1]
best_glm_FPrich <- get.models(all_glm_FPrich, 1)[1]

# but we actually want this to be the fitted model 
# I deleted a "+ 1" from the end of each of these formulas - not sure why it was there (MJM 8/27/2014) 
best_glm_LM <- glm(LM ~ ALK_avg + COND_avg + dist_waterfowl + latitude + secchi_avg, 
                         family = binomial, 
                         data = temp_data_LM, 
                         na.action = "na.fail")

best_glm_SP <- glm(SP ~ ALK_avg + dist_waterfowl + secchi_avg, 
                         family = binomial, 
                         data = temp_data_SP, 
                         na.action = "na.fail")

best_glm_W <- glm(W ~ boatlaunch + COND_avg + depth_max_m + latitude + surfacearea_ha + waterbodies_10km, 
                        family = binomial, 
                        data = temp_data_W, 
                        na.action = "na.fail")

best_glm_FPpres <- glm(FPpres ~ ALK_avg + COND_avg + dist_waterfowl + latitude + nearest_SP + secchi_avg + TOTP_avg,
                             family = binomial, 
                             data = temp_data_FPpres, 
                             na.action = "na.fail")

best_glm_FPrich <- glm(FPrich ~ ALK_avg + COND_avg + dist_waterfowl + latitude + nearest_SP + secchi_avg,
                             family = poisson, 
                             data = temp_data_FPrich, 
                             na.action = "na.fail")

summary(best_glm_LM)
summary(best_glm_SP) 
summary(best_glm_W) 
summary(best_glm_FPpres)
summary(best_glm_FPrich)

AIC(best_glm_LM)
AIC(best_glm_SP) 
AIC(best_glm_W) 
AIC(best_glm_FPpres)
AIC(best_glm_FPrich)

###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM <- model.avg(all_glm_LM, subset = delta < 2)
avg_SP <- model.avg(all_glm_SP, subset = delta < 2)
avg_W <- model.avg(all_glm_W, subset = delta < 2)
avg_FPpres <- model.avg(all_glm_FPpres, subset = delta < 2)
avg_FPrich <- model.avg(all_glm_FPrich, subset = delta < 2)

# Re-do the model averaging based on explicity specifying a model list 
# this is so I can use predict() with the model average object 
model_list_LM <- get.models(all_glm_LM, subset = delta < 2) # specify the list of models whose delta AIC < 2
model_list_SP <- get.models(all_glm_SP, subset = delta < 2)
model_list_W <- get.models(all_glm_W, subset = delta < 2)
model_list_FPpress <- get.models(all_glm_FPpres, subset = delta < 2)
model_list_FPrich <- get.models(all_glm_FPrich, subset = delta < 2)

avg_LM <- model.avg(model_list_LM) # use that list to do the model averaging 
avg_SP <- model.avg(model_list_SP)
avg_W <- model.avg(model_list_W)
avg_FPpres <- model.avg(model_list_FPpress)
avg_FPrich <- model.avg(model_list_FPrich)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM)
summary(avg_SP)
summary(avg_W)
summary(avg_FPpres)
summary(avg_FPrich)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM)
importance(avg_SP)
importance(avg_W)
importance(avg_FPpres)
importance(avg_FPrich)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM)
confint(avg_SP)
confint(avg_W)
confint(avg_FPpres)
confint(avg_FPrich)


######################
# pseudo-R-squared   #
# Variance explained #
# Best model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(best_glm_LM, null = glm_LM_null)
r.squaredLR(best_glm_SP, null = glm_SP_null)
r.squaredLR(best_glm_W, null = glm_W_null)
r.squaredLR(best_glm_FPpres, null = glm_FPpres_null)
r.squaredLR(best_glm_FPrich, null = glm_FPrich_null)

######################
# pseudo-R-squared   #
# Variance explained #
# Full model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(glm_LM, null = glm_LM_null)
r.squaredLR(glm_SP, null = glm_SP_null)
r.squaredLR(glm_W, null = glm_W_null)
r.squaredLR(glm_FPpres, null = glm_FPpres_null)
r.squaredLR(glm_FPrich, null = glm_FPrich_null)

####################### 
# Signif. of deviance #
# Full model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(glm_LM) # Residual Deviance
resid_df <- as.numeric(summary(glm_LM)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_SP) # Residual Deviance
resid_df <- as.numeric(summary(glm_SP)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_W) # Residual Deviance
resid_df <- as.numeric(summary(glm_W)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_FPpres) # Residual Deviance
resid_df <- as.numeric(summary(glm_FPpres)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_FPrich) # Residual Deviance
resid_df <- as.numeric(summary(glm_FPrich)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)


####################### 
# Signif. of deviance #
# Best model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(best_glm_LM) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_LM)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_SP) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_SP)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_W) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_W)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_FPpres) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_FPpres)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_FPrich) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_FPrich)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

######################
# Deviance explained #
# Full model         #
######################
null_dev <- as.numeric(summary(glm_LM)[8]) # Null Deviance      
resid_dev <- deviance(glm_LM) # Residual Deviance
glm_LM_devExpl <- (null_dev - resid_dev) / null_dev
glm_LM_devExpl

null_dev <- as.numeric(summary(glm_SP)[8]) # Null Deviance:      
resid_dev <- deviance(glm_SP) # Residual Deviance: 
glm_SP_devExpl <- (null_dev - resid_dev) / null_dev
glm_SP_devExpl

null_dev <- as.numeric(summary(glm_W)[8]) # Null Deviance: 
resid_dev <- deviance(glm_W) # Residual Deviance: 
glm_W_devExpl <- (null_dev - resid_dev) / null_dev
glm_W_devExpl

null_dev <- as.numeric(summary(glm_FPpres)[8]) # Null Deviance: 
resid_dev <- deviance(glm_FPpres) # Residual Deviance: 
glm_FPpres_devExpl <- (null_dev - resid_dev) / null_dev
glm_FPpres_devExpl

null_dev <- as.numeric(summary(glm_FPrich)[8]) # Null Deviance: 
resid_dev <- deviance(glm_FPrich) # Residual Deviance: 
glm_FPrich_devExpl <- (null_dev - resid_dev) / null_dev
glm_FPrich_devExpl

######################
# Deviance explained #
# Best model         #
######################
null_dev <- as.numeric(summary(best_glm_LM)[8]) # Null Deviance      
resid_dev <- deviance(best_glm_LM) # Residual Deviance
best_glm_LM_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_LM_devExpl

null_dev <- as.numeric(summary(best_glm_SP)[8]) # Null Deviance:      
resid_dev <- deviance(best_glm_SP) # Residual Deviance: 
best_glm_SP_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_SP_devExpl

null_dev <- as.numeric(summary(best_glm_W)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_W) # Residual Deviance: 
best_glm_W_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_W_devExpl

null_dev <- as.numeric(summary(best_glm_FPpres)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_FPpres) # Residual Deviance: 
best_glm_FPpres_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_FPpres_devExpl

null_dev <- as.numeric(summary(best_glm_FPrich)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_FPrich) # Residual Deviance: 
best_glm_FPrich_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_FPrich_devExpl

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

best_glm_LM_CV <- cv.glm(data=temp_data_LM, best_glm_LM, cost, K=10)
best_glm_LM_CV$delta

best_glm_SP_CV <- cv.glm(data=temp_data_SP, best_glm_SP, cost, K=10)
best_glm_SP_CV$delta

best_glm_W_CV <- cv.glm(data=temp_data_W, best_glm_W, cost, K=10)
best_glm_W_CV$delta

best_glm_FPpres_CV <- cv.glm(data=temp_data_FPpres, best_glm_FPpres, cost, K=10)
best_glm_FPpres_CV$delta

best_glm_FPrich_CV <- cv.glm(data=temp_data_FPrich, best_glm_FPrich, cost, K=10)
best_glm_FPrich_CV$delta

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

full_glm_LM_CV <- cv.glm(data=temp_data_LM, glm_LM, cost, K=10)
full_glm_LM_CV$delta

full_glm_SP_CV <- cv.glm(data=temp_data_SP, glm_SP, cost, K=10)
full_glm_SP_CV$delta

full_glm_W_CV <- cv.glm(data=temp_data_W, glm_W, cost, K=10)
full_glm_W_CV$delta

full_glm_FPpres_CV <- cv.glm(data=temp_data_FPpres, glm_FPpres, cost, K=10)
full_glm_FPpres_CV$delta

full_glm_FPrich_CV <- cv.glm(data=temp_data_FPrich, glm_FPrich, cost, K=10)
full_glm_FPrich_CV$delta

################# 
# Outlier Tests #
# Best model    #
#################
library(car)
outlierTest(best_glm_LM)
outlierTest(best_glm_SP)
outlierTest(best_glm_W)
outlierTest(best_glm_FPpres)
outlierTest(best_glm_FPrich)

################# 
# Outlier Tests #
# Full model    #
#################
library(car)
outlierTest(glm_LM)
outlierTest(glm_SP)
outlierTest(glm_W)
outlierTest(glm_FPpres)
outlierTest(glm_FPrich)

###################
# Residuals plots #
# Best model      #
###################
# If plots is curved - the model fit does not accurately describe the data 
library(car)
residualPlots(best_glm_LM)
residualPlots(best_glm_SP)
residualPlots(best_glm_W)
residualPlots(best_glm_FPpres)
residualPlots(best_glm_FPrich)

# returns lack-of-fit test is computed for each numeric predictor

jpeg("best_glm_LM_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_LM,main="Lemna minor - best model glm - residuals plot")
dev.off()

jpeg("best_glm_SP_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_SP,main="Spirodela polyrhiza - best model glm - residuals plot")
dev.off()

jpeg("best_glm_W_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_W,main="Wolffia sp. - best model glm -  residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_FPpres,main="FP presence - best model glm -  residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPrich_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_FPrich,main="FP richness - best model glm -  residuals plot",smoother=F)
dev.off()

##########################
# Partial residuals plot #
# Best model             # 
##########################
# effect of predictors given other predictors are also in the model (i.e., controlling for those predictors)
library(car)
crPlots(best_glm_LM)
crPlots(best_glm_SP)
crPlots(best_glm_W)
crPlots(best_glm_FPpres)
crPlots(best_glm_FPrich)

jpeg("best_glm_LM_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_LM,main="Lemna minor - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_SP_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_SP,main="Spirodela polyrhiza - best model glm - partial residuals plot",smoother=F,layout=1)
dev.off()

jpeg("best_glm_W_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_W,main="Wolffia sp. - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_FPpres,main="FP presence - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPrich_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_FPrich,main="FP presence - best model glm - partial residuals plot",smoother=F)
dev.off()

# or 

library(visreg)
visreg(best_glm_LM)
visreg(best_glm_SP)
visreg(best_glm_W)
visreg(best_glm_FPpres)
visreg(best_glm_FPrich)

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

best_glm_LM_resid <- resid(best_glm_LM)
best_glm_LM_resid <- as.vector(best_glm_LM_resid)

best_glm_SP_resid <- resid(best_glm_SP)
best_glm_SP_resid <- as.vector(best_glm_SP_resid)

best_glm_W_resid <- resid(best_glm_W)
best_glm_W_resid <- as.vector(best_glm_W_resid)

best_glm_FPpres_resid <- resid(best_glm_FPpres)
best_glm_FPpres_resid <- as.vector(best_glm_FPpres_resid)

best_glm_FPrich_resid <- resid(best_glm_FPrich)
best_glm_FPrich_resid <- as.vector(best_glm_FPrich_resid)


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
prediction <- ifelse(predict(best_glm_LM, temp_data_LM, type='response') > 0.5, "present", "absent")
# this makes the table of the predictions vs. the observed  
confusion  <- table(prediction, ifelse(best_glm_LM$y==1, "present", "absent"))
# I'm not sure what this classification error is so I'm going to shut it off 
# confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
# confusion  <- as.data.frame(confusion)
names(confusion) <- c('absent', 'present')
#names(confusion) <- c('FALSE', 'TRUE', 'class.error')
confusion

confusion.glm(temp_data_LM,best_glm_LM)
confusion.glm(temp_data_SP,best_glm_SP) # the model only predicts absences 
confusion.glm(temp_data_W,best_glm_W)
confusion.glm(temp_data_FPpres,best_glm_FPpres)

# cannot do it for FP richness - only works for logistic regression 
confusion.glm(temp_data_FPrich,best_glm_FPrich)

####################
# Confusion matrix #
# Model average    #
####################
# I cannot use the function that I defined above 
# a model.avg() object does not keep the observed values like a fitted.glm.object$y does 

# LM #
prediction <- ifelse(predict(avg_LM, temp_data_LM, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_LM$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# SP # 
prediction <- ifelse(predict(avg_SP, temp_data_SP, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_SP$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# W # 
prediction <- ifelse(predict(avg_W, temp_data_W, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_W$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# FPpres # 
prediction <- ifelse(predict(avg_FPpres, temp_data_FPpres, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_FPpres$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

###############################
# Generating predicted values #
###############################
predict(best_glm_LM, temp_data_LM, type="response")
predict(best_glm_SP, temp_data_SP, type="response")
predict(best_glm_W, temp_data_W, type="response")
predict(best_glm_FPrich, temp_data_FPrich, type="response")
predict(best_glm_FPpres, temp_data_FPpres, type="response")

############
# clean up #   
############
#rm(null_dev,resid_dev,temp_data)