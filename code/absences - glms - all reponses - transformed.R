########################
# GLMs                 #
# Y = species presence #
# 8/20/2014            #
########################

###############
# Lemna minor #
# Full & Null #
###############
# Build full and null models

colnames(dataENV_trans)
temp_data_LM <- dataENV_trans
temp_data_LM$nearest_SP <- NULL  # remove nearest SP
temp_data_LM$nearest_W <- NULL   # remove nearest W
temp_data_LM$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make the formula 
formula_LM <- as.formula(paste("LM ~ ", paste(colnames(temp_data_LM), collapse= "+")))
temp_data_LM$LM <- data$lemna_minor # add LM presence 
formula_LM # check out the formula 

# GLM for full model 
glm_LM_trans <- glm(formula_LM, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_trans)

# null model 
glm_LM_trans_null <- glm(LM ~ 1, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_trans_null)

#######################
# Spirodela polyrhiza #
# Full & Null         #
#######################
# Build full and null models

colnames(dataENV_trans)
temp_data_SP <- dataENV_trans
temp_data_SP$nearest_LM <- NULL  # remove nearest LM
temp_data_SP$nearest_W <- NULL   # remove nearest W
temp_data_SP$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_SP <- as.formula(paste("SP ~ ", paste(colnames(temp_data_SP), collapse= "+")))
temp_data_SP$SP <- data$spirodela_polyrhiza # add SP presence 
formula_SP # check out the formula 

# GLM for full model 
glm_SP_trans <- glm(formula_SP, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_trans)

# null model 
glm_SP_trans_null <- glm(SP ~ 1, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_trans_null)

###############
# Wolffia sp. #
# Full & Null #
###############
# Build full and null models

colnames(dataENV_trans)
temp_data_W <- dataENV_trans
temp_data_W$nearest_LM <- NULL  # remove nearest LM
temp_data_W$nearest_SP <- NULL   # remove nearest SP
temp_data_W$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W

# make formula 
formula_W <- as.formula(paste("W ~ ", paste(colnames(temp_data_W), collapse= "+")))
temp_data_W$W <- data$wolffia_sp # add W presence 
formula_W # view the formula 

# GLM for full model 
glm_W_trans <- glm(formula_W, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_trans)

# null model 
glm_W_trans_null <- glm(W ~ 1, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_trans_null)

###############
# FP presence # 
# Full & Null #
###############
# Build full and null models

colnames(dataENV_trans)
temp_data_FPpres <- dataENV_trans
temp_data_FPpres$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPpres)

# make formula 
formula_FPpres <- as.formula(paste("FPpres ~ ", paste(colnames(temp_data_FPpres), collapse= "+")))
temp_data_FPpres$FPpres <- as.numeric(data$FP_presence)-1 # add FP ppresence 
formula_FPpres # view the formula 

# GLM for full model 
glm_FPpres_trans <- glm(formula_FPpres, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_trans)

# null model 
glm_FPpres_trans_null <- glm(FPpres ~ 1, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_trans_null)

###############
# FP richness # 
# Full & Null #
###############
# Build full and null models

colnames(dataENV_trans)
temp_data_FPrich <- dataENV_trans
temp_data_FPrich$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W
colnames(temp_data_FPrich)

# make formula 
formula_FPrich <- as.formula(paste("FPrich ~ ", paste(colnames(temp_data_FPrich), collapse= "+")))
temp_data_FPrich$FPrich <- data$FP_species_richness # add FP ppresence 
formula_FPrich # view the formula 

# GLM for full model 
glm_FPrich_trans <- glm(formula_FPrich, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_trans)

# null model 
glm_FPrich_trans_null <- glm(FPrich ~ 1, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_trans_null)

############################
# AICs for all full models #
############################
AIC(glm_LM_trans)
AIC(glm_SP_trans) 
AIC(glm_W_trans) 
AIC(glm_FPpres_trans)
AIC(glm_FPrich_trans)

############################
# AICs for all null models #
############################
AIC(glm_LM_trans_null)
AIC(glm_SP_trans_null) 
AIC(glm_W_trans_null) 
AIC(glm_FPpres_trans_null)
AIC(glm_FPrich_trans_null)


#########################
# Run all nested models #
#########################
library(MuMIn)

# this may take awhile 
all_glm_LM_trans <- dredge(glm_LM_trans)
all_glm_SP_trans <- dredge(glm_SP_trans)
all_glm_W_trans <- dredge(glm_W_trans)
all_glm_FPpres_trans <- dredge(glm_FPpres_trans)
all_glm_FPrich_trans <- dredge(glm_FPrich_trans)

# save to file 
numb_models <- nrow(all_glm_LM_trans)
write.csv(all_glm_LM_trans[1:numb_models],file="all_glm_LM_trans_2.csv",na="NA")
numb_models <- nrow(all_glm_SP_trans)
write.csv(all_glm_SP_trans[1:numb_models],file="all_glm_SP_trans_2.csv",na="NA")
numb_models <- nrow(all_glm_W_trans)
write.csv(all_glm_W_trans[1:numb_models],file="all_glm_W_trans_2.csv",na="NA")
numb_models <- nrow(all_glm_FPpres_trans)
write.csv(all_glm_FPpres_trans[1:numb_models],file="all_glm_FPpres_trans_2.csv",na="NA")
numb_models <- nrow(all_glm_FPrich_trans)
write.csv(all_glm_FPrich_trans[1:numb_models],file="all_glm_FPrich_trans_2.csv",na="NA")

##############
# Best model #  
##############
library(MuMIn)

# assign the best model to an object 
best_glm_LM_trans <- get.models(all_glm_LM_trans, 1)[1]
best_glm_SP_trans <- get.models(all_glm_SP_trans, 1)[1]
best_glm_W_trans <- get.models(all_glm_W_trans, 1)[1]
best_glm_FPpres_trans <- get.models(all_glm_FPpres_trans, 1)[1]
best_glm_FPrich_trans <- get.models(all_glm_FPrich_trans, 1)[1]

# but we actually want this to be the fitted model 
# I deleted a "+ 1" from the end of each of these formulas - not sure why it was there (MJM 8/27/2014) 
best_glm_LM_trans <- glm(LM ~ COND_avg + dist_waterfowl + latitude + nearest_LM + secchi_avg + TOTP_avg + waterbodies_1km, 
                         family = binomial, 
                         data = temp_data_LM, 
                         na.action = "na.fail")

best_glm_SP_trans <- glm(SP ~ COND_avg, family = binomial, data = temp_data_SP, 
                         na.action = "na.fail")

best_glm_W_trans <- glm(W ~ boatlaunch + COND_avg + secchi_avg, family = binomial, 
                        data = temp_data_W, na.action = "na.fail")

best_glm_FPpres_trans <- glm(FPpres ~ COND_avg + latitude + nearest_LM + nearest_SP + secchi_avg + TOTP_avg,
                             family = binomial, 
                             data = temp_data_FPpres, 
                             na.action = "na.fail")

best_glm_FPrich_trans <- glm(FPrich ~ COND_avg + dist_waterfowl + latitude + nearest_LM + secchi_avg + TOTP_avg + waterbodies_1km,
                             family = poisson, 
                             data = temp_data_FPrich, 
                             na.action = "na.fail")

summary(best_glm_LM_trans)
summary(best_glm_SP_trans) 
summary(best_glm_W_trans) 
summary(best_glm_FPpres_trans)
summary(best_glm_FPrich_trans)

AIC(best_glm_LM_trans)
AIC(best_glm_SP_trans) 
AIC(best_glm_W_trans) 
AIC(best_glm_FPpres_trans)
AIC(best_glm_FPrich_trans)

###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM_trans <- model.avg(all_glm_LM_trans, subset = delta < 2)
avg_SP_trans <- model.avg(all_glm_SP_trans, subset = delta < 2)
avg_W_trans <- model.avg(all_glm_W_trans, subset = delta < 2)
avg_FPpres_trans <- model.avg(all_glm_FPpres_trans, subset = delta < 2)
avg_FPrich_trans <- model.avg(all_glm_FPrich_trans, subset = delta < 2)

# Re-do the model averaging based on explicity specifying a model list 
# this is so I can use predict() with the model average object 
model_list_LM <- get.models(all_glm_LM_trans, subset = delta < 2) # specify the list of models whose delta AIC < 2
model_list_SP <- get.models(all_glm_SP_trans, subset = delta < 2)
model_list_W <- get.models(all_glm_W_trans, subset = delta < 2)
model_list_FPpress <- get.models(all_glm_FPpres_trans, subset = delta < 2)
model_list_FPrich <- get.models(all_glm_FPrich_trans, subset = delta < 2)

avg_LM_trans <- model.avg(model_list_LM) # use that list to do the model averaging 
avg_SP_trans <- model.avg(model_list_SP)
avg_W_trans <- model.avg(model_list_W)
avg_FPpres_trans <- model.avg(model_list_FPpress)
avg_FPrich_trans <- model.avg(model_list_FPrich)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM_trans)
summary(avg_SP_trans)
summary(avg_W_trans)
summary(avg_FPpres_trans)
summary(avg_FPrich_trans)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM_trans)
importance(avg_SP_trans)
importance(avg_W_trans)
importance(avg_FPpres_trans)
importance(avg_FPrich_trans)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM_trans)
confint(avg_SP_trans)
confint(avg_W_trans)
confint(avg_FPpres_trans)
confint(avg_FPrich_trans)


######################
# pseudo-R-squared   #
# Variance explained #
# Best model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(best_glm_LM_trans, null = glm_LM_trans_null)
r.squaredLR(best_glm_SP_trans, null = glm_SP_trans_null)
r.squaredLR(best_glm_W_trans, null = glm_W_trans_null)
r.squaredLR(best_glm_FPpres_trans, null = glm_FPpres_trans_null)
r.squaredLR(best_glm_FPrich_trans, null = glm_FPrich_trans_null)

######################
# pseudo-R-squared   #
# Variance explained #
# Full model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(glm_LM_trans, null = glm_LM_trans_null)
r.squaredLR(glm_SP_trans, null = glm_SP_trans_null)
r.squaredLR(glm_W_trans, null = glm_W_trans_null)
r.squaredLR(glm_FPpres_trans, null = glm_FPpres_trans_null)
r.squaredLR(glm_FPrich_trans, null = glm_FPrich_trans_null)

####################### 
# Signif. of deviance #
# Full model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(glm_LM_trans) # Residual Deviance
resid_df <- as.numeric(summary(glm_LM_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_SP_trans) # Residual Deviance
resid_df <- as.numeric(summary(glm_SP_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_W_trans) # Residual Deviance
resid_df <- as.numeric(summary(glm_W_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_FPpres_trans) # Residual Deviance
resid_df <- as.numeric(summary(glm_FPpres_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_FPrich_trans) # Residual Deviance
resid_df <- as.numeric(summary(glm_FPrich_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)


####################### 
# Signif. of deviance #
# Best model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(best_glm_LM_trans) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_LM_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_SP_trans) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_SP_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_W_trans) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_W_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_FPpres_trans) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_FPpres_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_FPrich_trans) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_FPrich_trans)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

######################
# Deviance explained #
# Full model         #
######################
null_dev <- as.numeric(summary(glm_LM_trans)[8]) # Null Deviance      
resid_dev <- deviance(glm_LM_trans) # Residual Deviance
glm_LM_trans_devExpl <- (null_dev - resid_dev) / null_dev
glm_LM_trans_devExpl

null_dev <- as.numeric(summary(glm_SP_trans)[8]) # Null Deviance:      
resid_dev <- deviance(glm_SP_trans) # Residual Deviance: 
glm_SP_trans_devExpl <- (null_dev - resid_dev) / null_dev
glm_SP_trans_devExpl

null_dev <- as.numeric(summary(glm_W_trans)[8]) # Null Deviance: 
resid_dev <- deviance(glm_W_trans) # Residual Deviance: 
glm_W_trans_devExpl <- (null_dev - resid_dev) / null_dev
glm_W_trans_devExpl

null_dev <- as.numeric(summary(glm_FPpres_trans)[8]) # Null Deviance: 
resid_dev <- deviance(glm_FPpres_trans) # Residual Deviance: 
glm_FPpres_trans_devExpl <- (null_dev - resid_dev) / null_dev
glm_FPpres_trans_devExpl

null_dev <- as.numeric(summary(glm_FPrich_trans)[8]) # Null Deviance: 
resid_dev <- deviance(glm_FPrich_trans) # Residual Deviance: 
glm_FPrich_trans_devExpl <- (null_dev - resid_dev) / null_dev
glm_FPrich_trans_devExpl

######################
# Deviance explained #
# Best model         #
######################
null_dev <- as.numeric(summary(best_glm_LM_trans)[8]) # Null Deviance      
resid_dev <- deviance(best_glm_LM_trans) # Residual Deviance
best_glm_LM_trans_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_LM_trans_devExpl

null_dev <- as.numeric(summary(best_glm_SP_trans)[8]) # Null Deviance:      
resid_dev <- deviance(best_glm_SP_trans) # Residual Deviance: 
best_glm_SP_trans_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_SP_trans_devExpl

null_dev <- as.numeric(summary(best_glm_W_trans)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_W_trans) # Residual Deviance: 
best_glm_W_trans_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_W_trans_devExpl

null_dev <- as.numeric(summary(best_glm_FPpres_trans)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_FPpres_trans) # Residual Deviance: 
best_glm_FPpres_trans_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_FPpres_trans_devExpl

null_dev <- as.numeric(summary(best_glm_FPrich_trans)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_FPrich_trans) # Residual Deviance: 
best_glm_FPrich_trans_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_FPrich_trans_devExpl

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

best_glm_LM_trans_CV <- cv.glm(data=temp_data_LM, best_glm_LM_trans, cost, K=10)
best_glm_LM_trans_CV$delta

best_glm_SP_trans_CV <- cv.glm(data=temp_data_SP, best_glm_SP_trans, cost, K=10)
best_glm_SP_trans_CV$delta

best_glm_W_trans_CV <- cv.glm(data=temp_data_W, best_glm_W_trans, cost, K=10)
best_glm_W_trans_CV$delta

best_glm_FPpres_trans_CV <- cv.glm(data=temp_data_FPpres, best_glm_FPpres_trans, cost, K=10)
best_glm_FPpres_trans_CV$delta

best_glm_FPrich_trans_CV <- cv.glm(data=temp_data_FPrich, best_glm_FPrich_trans, cost, K=10)
best_glm_FPrich_trans_CV$delta

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

full_glm_LM_trans_CV <- cv.glm(data=temp_data_LM, glm_LM_trans, cost, K=10)
full_glm_LM_trans_CV$delta

full_glm_SP_trans_CV <- cv.glm(data=temp_data_SP, glm_SP_trans, cost, K=10)
full_glm_SP_trans_CV$delta

full_glm_W_trans_CV <- cv.glm(data=temp_data_W, glm_W_trans, cost, K=10)
full_glm_W_trans_CV$delta

full_glm_FPpres_trans_CV <- cv.glm(data=temp_data_FPpres, glm_FPpres_trans, cost, K=10)
full_glm_FPpres_trans_CV$delta

full_glm_FPrich_trans_CV <- cv.glm(data=temp_data_FPrich, glm_FPrich_trans, cost, K=10)
full_glm_FPrich_trans_CV$delta

################# 
# Outlier Tests #
# Best model    #
#################
library(car)
outlierTest(best_glm_LM_trans)
outlierTest(best_glm_SP_trans)
outlierTest(best_glm_W_trans)
outlierTest(best_glm_FPpres_trans)
outlierTest(best_glm_FPrich_trans)

################# 
# Outlier Tests #
# Full model    #
#################
library(car)
outlierTest(glm_LM_trans)
outlierTest(glm_SP_trans)
outlierTest(glm_W_trans)
outlierTest(glm_FPpres_trans)
outlierTest(glm_FPrich_trans)

###################
# Residuals plots #
# Best model      #
###################
# If plots is curved - the model fit does not accurately describe the data 
library(car)
residualPlots(best_glm_LM_trans)
residualPlots(best_glm_SP_trans)
residualPlots(best_glm_W_trans)
residualPlots(best_glm_FPpres_trans)
residualPlots(best_glm_FPrich_trans)

# returns lack-of-fit test is computed for each numeric predictor

jpeg("best_glm_LM_trans_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_LM_trans,main="Lemna minor - best model glm - residuals plot")
dev.off()

jpeg("best_glm_SP_trans_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_SP_trans,main="Spirodela polyrhiza - best model glm - residuals plot")
dev.off()

jpeg("best_glm_W_trans_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_W_trans,main="Wolffia sp. - best model glm -  residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_trans_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_FPpres_trans,main="FP presence - best model glm -  residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPrich_trans_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_FPrich_trans,main="FP richness - best model glm -  residuals plot",smoother=F)
dev.off()

##########################
# Partial residuals plot #
# Best model             # 
##########################
# effect of predictors given other predictors are also in the model (i.e., controlling for those predictors)
library(car)
crPlots(best_glm_LM_trans)
crPlots(best_glm_SP_trans)
crPlots(best_glm_W_trans)
crPlots(best_glm_FPpres_trans)
crPlots(best_glm_FPrich_trans)

jpeg("best_glm_LM_trans_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_LM_trans,main="Lemna minor - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_SP_trans_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_SP_trans,main="Spirodela polyrhiza - best model glm - partial residuals plot",smoother=F,layout=1)
dev.off()

jpeg("best_glm_W_trans_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_W_trans,main="Wolffia sp. - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPpres_trans_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_FPpres_trans,main="FP presence - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_FPrich_trans_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_FPrich_trans,main="FP presence - best model glm - partial residuals plot",smoother=F)
dev.off()

# or 

library(visreg)
visreg(best_glm_LM_trans)
visreg(best_glm_SP_trans)
visreg(best_glm_W_trans)
visreg(best_glm_FPpres_trans)
visreg(best_glm_FPrich_trans)

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

best_glm_LM_trans_resid <- resid(best_glm_LM_trans)
best_glm_LM_trans_resid <- as.vector(best_glm_LM_trans_resid)

best_glm_SP_trans_resid <- resid(best_glm_SP_trans)
best_glm_SP_trans_resid <- as.vector(best_glm_SP_trans_resid)

best_glm_W_trans_resid <- resid(best_glm_W_trans)
best_glm_W_trans_resid <- as.vector(best_glm_W_trans_resid)

best_glm_FPpres_trans_resid <- resid(best_glm_FPpres_trans)
best_glm_FPpres_trans_resid <- as.vector(best_glm_FPpres_trans_resid)

best_glm_FPrich_trans_resid <- resid(best_glm_FPrich_trans)
best_glm_FPrich_trans_resid <- as.vector(best_glm_FPrich_trans_resid)


####################################
# Spatial correlation of residuals #
####################################
library(spdep)
sp.correlogram()

# need a matrix of distances between all pairs 
# I used a different package and functions last time - compare to this 


####################
# Confusion matrix #
<<<<<<< HEAD
<<<<<<< HEAD
# Best model       #
=======
>>>>>>> 66da6cb... added confusion matrices for logistic regressions
=======
# Best model       #
>>>>>>> d4f2a63... model averaging
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

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> d4f2a63... model averaging
# try with real data 
prediction <- ifelse(predict(best_glm_LM_trans, temp_data_LM, type='response') > 0.5, "present", "absent")
# this makes the table of the predictions vs. the observed  
confusion  <- table(prediction, ifelse(best_glm_LM_trans$y==1, "present", "absent"))
# I'm not sure what this classification error is so I'm going to shut it off 
# confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
# confusion  <- as.data.frame(confusion)
names(confusion) <- c('absent', 'present')
#names(confusion) <- c('FALSE', 'TRUE', 'class.error')
confusion

<<<<<<< HEAD
=======
>>>>>>> 66da6cb... added confusion matrices for logistic regressions
=======
>>>>>>> d4f2a63... model averaging
confusion.glm(temp_data_LM,best_glm_LM_trans)
confusion.glm(temp_data_SP,best_glm_SP_trans) # the model only predicts absences 
confusion.glm(temp_data_W,best_glm_W_trans)
confusion.glm(temp_data_FPpres,best_glm_FPpres_trans)

# cannot do it for FP richness - only works for logistic regression 
confusion.glm(temp_data_FPrich,best_glm_FPrich_trans)

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> d4f2a63... model averaging
####################
# Confusion matrix #
# Model average    #
####################
# I cannot use the function that I defined above 
# a model.avg() object does not keep the observed values like a fitted.glm.object$y does 

# LM #
prediction <- ifelse(predict(avg_LM_trans, temp_data_LM, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_LM_trans$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# SP # 
prediction <- ifelse(predict(avg_SP_trans, temp_data_SP, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_SP_trans$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# W # 
prediction <- ifelse(predict(avg_W_trans, temp_data_W, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_W_trans$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

# FPpres # 
prediction <- ifelse(predict(avg_FPpres_trans, temp_data_FPpres, type='response') > 0.5, "present", "absent")
confusion  <- table(prediction, ifelse(best_glm_FPpres_trans$y==1, "present", "absent"))
names(confusion) <- c('absent', 'present')
confusion

<<<<<<< HEAD
=======
>>>>>>> 66da6cb... added confusion matrices for logistic regressions
=======
>>>>>>> d4f2a63... model averaging
###############################
# Generating predicted values #
###############################
predict(best_glm_LM_trans, temp_data_LM, type="response")
predict(best_glm_SP_trans, temp_data_SP, type="response")
predict(best_glm_W_trans, temp_data_W, type="response")
predict(best_glm_FPrich_trans, temp_data_FPrich, type="response")
predict(best_glm_FPpres_trans, temp_data_FPpres, type="response")

############
# clean up #   
############
#rm(null_dev,resid_dev,temp_data)