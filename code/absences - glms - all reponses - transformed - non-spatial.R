########################
# GLMs                 #
# Y = species presence #
# 8/29/2014            #
# Non-spatial glms     #
########################

# remove spatial variables 
colnames(dataENV_trans)
dataENV_trans_noSPACE <- dataENV_trans
dataENV_trans_noSPACE$waterbodies_1km <- NULL
dataENV_trans_noSPACE$waterbodies_10km <- NULL
dataENV_trans_noSPACE$nearest_LM <- NULL
dataENV_trans_noSPACE$nearest_SP <- NULL
dataENV_trans_noSPACE$nearest_W <- NULL
dataENV_trans_noSPACE$latitude <- NULL
dataENV_trans_noSPACE$longitude <- NULL
dataENV_trans_noSPACE$boatlaunch <- NULL
dataENV_trans_noSPACE$dist_waterfowl <- NULL
colnames(dataENV_trans_noSPACE)

###############
# Lemna minor #
# Full & Null #
###############
# Build full and null models

# clean-up data
temp_data_LM <- dataENV_trans_noSPACE
temp_data_LM$nearest_SP <- NULL  # remove nearest SP
temp_data_LM$nearest_W <- NULL   # remove nearest W

# make the formula 
formula_LM <- as.formula(paste("LM ~ ", paste(colnames(temp_data_LM), collapse= "+")))
temp_data_LM$LM <- data$lemna_minor # add LM presence 
formula_LM # check out the formula 

# GLM for full model 
glm_LM_trans_noSPACE <- glm(formula_LM, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_trans_noSPACE)

# null model 
glm_LM_trans_noSPACE_null <- glm(LM ~ 1, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_trans_noSPACE_null)

#######################
# Spirodela polyrhiza #
# Full & Null         #
#######################
# Build full and null models

# clean-up data
colnames(dataENV_trans_noSPACE)
temp_data_SP <- dataENV_trans_noSPACE
temp_data_SP$nearest_LM <- NULL  # remove nearest LM
temp_data_SP$nearest_W <- NULL   # remove nearest W

# make formula 
formula_SP <- as.formula(paste("SP ~ ", paste(colnames(temp_data_SP), collapse= "+")))
temp_data_SP$SP <- data$spirodela_polyrhiza # add SP presence 
formula_SP # check out the formula 

# GLM for full model 
glm_SP_trans_noSPACE <- glm(formula_SP, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_trans_noSPACE)

# null model 
glm_SP_trans_noSPACE_null <- glm(SP ~ 1, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_trans_noSPACE_null)

###############
# Wolffia sp. #
# Full & Null #
###############
# Build full and null models

# clean-up data
colnames(dataENV_trans_noSPACE)
temp_data_W <- dataENV_trans_noSPACE
temp_data_W$nearest_LM <- NULL  # remove nearest LM
temp_data_W$nearest_SP <- NULL   # remove nearest SP

# make formula 
formula_W <- as.formula(paste("W ~ ", paste(colnames(temp_data_W), collapse= "+")))
temp_data_W$W <- data$wolffia_sp # add W presence 
formula_W # view the formula 

# GLM for full model 
glm_W_trans_noSPACE <- glm(formula_W, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_trans_noSPACE)

# null model 
glm_W_trans_noSPACE_null <- glm(W ~ 1, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_trans_noSPACE_null)

#########################
# Run all nested models #
#########################
library(MuMIn)

# this may take awhile 
all_glm_LM_trans_noSPACE <- dredge(glm_LM_trans_noSPACE)
all_glm_SP_trans_noSPACE <- dredge(glm_SP_trans_noSPACE)
all_glm_W_trans_noSPACE <- dredge(glm_W_trans_noSPACE)

# save to file 
numb_models <- nrow(all_glm_LM_trans_noSPACE)
write.csv(all_glm_LM_trans_noSPACE[1:numb_models],file="all_glm_LM_trans_noSPACE.csv",na="NA")
numb_models <- nrow(all_glm_SP_trans_noSPACE)
write.csv(all_glm_SP_trans_noSPACE[1:numb_models],file="all_glm_SP_trans_noSPACE.csv",na="NA")
numb_models <- nrow(all_glm_W_trans_noSPACE)
write.csv(all_glm_W_trans_noSPACE[1:numb_models],file="all_glm_W_trans_noSPACE.csv",na="NA")

##############
# Best model #  
##############
library(MuMIn)

# assign the best model to an object 
best_glm_LM_trans_noSPACE <- get.models(all_glm_LM_trans_noSPACE, 1)[1]
best_glm_SP_trans_noSPACE <- get.models(all_glm_SP_trans_noSPACE, 1)[1]
best_glm_W_trans_noSPACE <- get.models(all_glm_W_trans_noSPACE, 1)[1]

# but we actually want this to be the fitted model 
# I deleted a "+ 1" from the end of each of these formulas - not sure why it was there (MJM 8/27/2014) 
best_glm_LM_trans_noSPACE <- glm(LM ~ COND_avg + secchi_avg + TOTP_avg, 
                                 family = binomial, 
                                 data = temp_data_LM, 
                                 na.action = "na.fail")

best_glm_SP_trans_noSPACE <- glm(SP ~ COND_avg, 
                                 family = binomial, 
                                 data = temp_data_SP, 
                                 na.action = "na.fail")

best_glm_W_trans_noSPACE <- glm(W ~ COND_avg + secchi_avg, 
                                family = binomial, 
                                data = temp_data_W, 
                                na.action = "na.fail")

###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM_trans_noSPACE <- model.avg(all_glm_LM_trans_noSPACE, subset = delta < 2)
avg_SP_trans_noSPACE <- model.avg(all_glm_SP_trans_noSPACE, subset = delta < 2)
avg_W_trans_noSPACE <- model.avg(all_glm_W_trans_noSPACE, subset = delta < 2)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM_trans_noSPACE)
summary(avg_SP_trans_noSPACE)
summary(avg_W_trans_noSPACE)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM_trans_noSPACE)
importance(avg_SP_trans_noSPACE)
importance(avg_W_trans_noSPACE)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM_trans_noSPACE)
confint(avg_SP_trans_noSPACE)
confint(avg_W_trans_noSPACE)

######################
# pseudo-R-squared   #
# Variance explained #
# Best model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(best_glm_LM_trans_noSPACE, null = glm_LM_trans_noSPACE_null)
r.squaredLR(best_glm_SP_trans_noSPACE, null = glm_SP_trans_noSPACE_null)
r.squaredLR(best_glm_W_trans_noSPACE, null = glm_W_trans_noSPACE_null)

######################
# pseudo-R-squared   #
# Variance explained #
# Ful model          #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(glm_LM_trans_noSPACE, null = glm_LM_trans_noSPACE_null)
r.squaredLR(glm_SP_trans_noSPACE, null = glm_SP_trans_noSPACE_null)
r.squaredLR(glm_W_trans_noSPACE, null = glm_W_trans_noSPACE_null)

####################### 
# Signif. of deviance #
# Full model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(glm_LM_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(glm_LM_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_SP_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(glm_SP_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(glm_W_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(glm_W_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

####################### 
# Signif. of deviance #
# Best model          #
#######################
# if significant (p<0.05), then model is poor fit to data 
resid_dev <- deviance(best_glm_LM_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_LM_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_SP_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_SP_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

resid_dev <- deviance(best_glm_W_trans_noSPACE) # Residual Deviance
resid_df <- as.numeric(summary(best_glm_W_trans_noSPACE)[7]) # Null Deviance    
1-pchisq(resid_dev,resid_df)

######################
# Deviance explained #
# Full model         #
######################
null_dev <- as.numeric(summary(glm_LM_trans_noSPACE)[8]) # Null Deviance      
resid_dev <- deviance(glm_LM_trans_noSPACE) # Residual Deviance
glm_LM_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
glm_LM_trans_noSPACE_devExpl

null_dev <- as.numeric(summary(glm_SP_trans_noSPACE)[8]) # Null Deviance:      
resid_dev <- deviance(glm_SP_trans_noSPACE) # Residual Deviance: 
glm_SP_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
glm_SP_trans_noSPACE_devExpl

null_dev <- as.numeric(summary(glm_W_trans_noSPACE)[8]) # Null Deviance: 
resid_dev <- deviance(glm_W_trans_noSPACE) # Residual Deviance: 
glm_W_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
glm_W_trans_noSPACE_devExpl

######################
# Deviance explained #
# Best model         #
######################
null_dev <- as.numeric(summary(best_glm_LM_trans_noSPACE)[8]) # Null Deviance      
resid_dev <- deviance(best_glm_LM_trans_noSPACE) # Residual Deviance
best_glm_LM_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_LM_trans_noSPACE_devExpl

null_dev <- as.numeric(summary(best_glm_SP_trans_noSPACE)[8]) # Null Deviance:      
resid_dev <- deviance(best_glm_SP_trans_noSPACE) # Residual Deviance: 
best_glm_SP_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_SP_trans_noSPACE_devExpl

null_dev <- as.numeric(summary(best_glm_W_trans_noSPACE)[8]) # Null Deviance: 
resid_dev <- deviance(best_glm_W_trans_noSPACE) # Residual Deviance: 
best_glm_W_trans_noSPACE_devExpl <- (null_dev - resid_dev) / null_dev
best_glm_W_trans_noSPACE_devExpl

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

best_glm_LM_trans_noSPACE_CV <- cv.glm(data=temp_data_LM, best_glm_LM_trans_noSPACE, cost, K=10)
best_glm_LM_trans_noSPACE_CV$delta

best_glm_SP_trans_noSPACE_CV <- cv.glm(data=temp_data_SP, best_glm_SP_trans_noSPACE, cost, K=10)
best_glm_SP_trans_noSPACE_CV$delta

best_glm_W_trans_noSPACE_CV <- cv.glm(data=temp_data_W, best_glm_W_trans_noSPACE, cost, K=10)
best_glm_W_trans_noSPACE_CV$delta

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

full_glm_LM_trans_noSPACE_CV <- cv.glm(data=temp_data_LM, glm_LM_trans_noSPACE, cost, K=10)
full_glm_LM_trans_noSPACE_CV$delta

full_glm_SP_trans_noSPACE_CV <- cv.glm(data=temp_data_SP, glm_SP_trans_noSPACE, cost, K=10)
full_glm_SP_trans_noSPACE_CV$delta

full_glm_W_trans_noSPACE_CV <- cv.glm(data=temp_data_W, glm_W_trans_noSPACE, cost, K=10)
full_glm_W_trans_noSPACE_CV$delta


################# 
# Outlier Tests #
# Best model    #
#################
library(car)
outlierTest(best_glm_LM_trans_noSPACE)
outlierTest(best_glm_SP_trans_noSPACE)
outlierTest(best_glm_W_trans_noSPACE)

################# 
# Outlier Tests #
# Full model    #
#################
library(car)
outlierTest(glm_LM_trans_noSPACE)
outlierTest(glm_SP_trans_noSPACE)
outlierTest(glm_W_trans_noSPACE)


###################
# Residuals plots #
# Best model      #
###################
# If plots is curved - the model fit does not accurately describe the data 
library(car)
residualPlots(best_glm_LM_trans_noSPACE)
residualPlots(best_glm_SP_trans_noSPACE)
residualPlots(best_glm_W_trans_noSPACE)

# returns lack-of-fit test is computed for each numeric predictor

jpeg("best_glm_LM_trans_noSPACE_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_LM_trans_noSPACE,main="Lemna minor - best model glm - residuals plot")
dev.off()

jpeg("best_glm_SP_trans_noSPACE_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_SP_trans_noSPACE,main="Spirodela polyrhiza - best model glm - residuals plot")
dev.off()

jpeg("best_glm_W_trans_noSPACE_resids_plot.jpg",height=8,width=8,units="in",res=300)
residualPlots(best_glm_W_trans_noSPACE,main="Wolffia sp. - best model glm -  residuals plot",smoother=F)
dev.off()


##########################
# Partial residuals plot #
# Best model             # 
##########################
# effect of predictors given other predictors are also in the model (i.e., controlling for those predictors)
library(car)
crPlots(best_glm_LM_trans_noSPACE)
crPlots(best_glm_SP_trans_noSPACE)
crPlots(best_glm_W_trans_noSPACE)

jpeg("best_glm_LM_trans_noSPACE_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_LM_trans_noSPACE,main="Lemna minor - best model glm - partial residuals plot",smoother=F)
dev.off()

jpeg("best_glm_SP_trans_noSPACE_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_SP_trans_noSPACE,main="Spirodela polyrhiza - best model glm - partial residuals plot",smoother=F,layout=1)
dev.off()

jpeg("best_glm_W_trans_noSPACE_partial_resids_plot.jpg",height=8,width=8,units="in",res=300)
crPlots(best_glm_W_trans_noSPACE,main="Wolffia sp. - best model glm - partial residuals plot",smoother=F)
dev.off()

# or 

library(visreg)
visreg(best_glm_LM_trans_noSPACE)
visreg(best_glm_SP_trans_noSPACE)
visreg(best_glm_W_trans_noSPACE)

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

best_glm_LM_trans_noSPACE_resid <- resid(best_glm_LM_trans_noSPACE)
best_glm_LM_trans_noSPACE_resid <- as.vector(best_glm_LM_trans_noSPACE_resid)

best_glm_SP_trans_noSPACE_resid <- resid(best_glm_SP_trans_noSPACE)
best_glm_SP_trans_noSPACE_resid <- as.vector(best_glm_SP_trans_noSPACE_resid)

best_glm_W_trans_noSPACE_resid <- resid(best_glm_W_trans_noSPACE)
best_glm_W_trans_noSPACE_resid <- as.vector(best_glm_W_trans_noSPACE_resid)


############
# clean up #   
############
#rm(null_dev,resid_dev,temp_data)