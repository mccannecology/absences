load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - data imported.RData")
# or 
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5.RData")

###############
# Lemna minor #
# Full & Null #
###############
# Build full and null models
temp_data_LM <- dataENV_scaled_out
temp_data_LM$nearest_SP <- NULL  # remove nearest SP
temp_data_LM$nearest_W <- NULL   # remove nearest W
temp_data_LM$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_LM$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W
temp_data_LM$ALK_avg <- NULL   # remove ALK_avg 
temp_data_LM$depth_max_m <- NULL   # remove depth_max_m 
temp_data_LM$dist_waterfowl <- NULL   # remove dist_waterfowl 

# make the formula 
formula_LM <- as.formula(paste("LM ~ ", paste(colnames(temp_data_LM), collapse= "+")))
temp_data_LM$LM <- data$lemna_minor[-129] # add LM presence 
formula_LM # check out the formula 

# GLM for full model 
glm_LM_scaled_out <- glm(formula_LM, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_scaled_out)

# null model 
glm_LM_scaled_out_null <- glm(LM ~ 1, family=binomial, data=temp_data_LM, na.action = "na.fail")
summary(glm_LM_scaled_out_null)

#######################
# Spirodela polyrhiza #
# Full & Null         #
#######################
# Build full and null models
temp_data_SP <- dataENV_scaled_out
temp_data_SP$nearest_LM <- NULL  # remove nearest LM
temp_data_SP$nearest_W <- NULL   # remove nearest W
temp_data_SP$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_SP$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W
temp_data_SP$ALK_avg <- NULL   # remove ALK_avg 
temp_data_SP$depth_max_m <- NULL   # remove depth_max_m 
temp_data_SP$dist_waterfowl <- NULL   # remove dist_waterfowl 

# make formula 
formula_SP <- as.formula(paste("SP ~ ", paste(colnames(temp_data_SP), collapse= "+")))
temp_data_SP$SP <- data$spirodela_polyrhiza[-129] # add SP presence 
formula_SP # check out the formula 

# GLM for full model 
glm_SP_scaled_out <- glm(formula_SP, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_scaled_out)

# null model 
glm_SP_scaled_out_null <- glm(SP ~ 1, family=binomial, data=temp_data_SP, na.action = "na.fail")
summary(glm_SP_scaled_out_null)

###############
# Wolffia sp. #
# Full & Null #
###############
# Build full and null models
temp_data_W <- dataENV_scaled_out
temp_data_W$nearest_LM <- NULL  # remove nearest LM
temp_data_W$nearest_SP <- NULL   # remove nearest SP
temp_data_W$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_W$nearest_LMSPW <- NULL   # remove nearest LM,SP,or W
temp_data_W$ALK_avg <- NULL   # remove ALK_avg 
temp_data_W$depth_max_m <- NULL   # remove depth_max_m 
temp_data_W$dist_waterfowl <- NULL   # remove dist_waterfowl 


# make formula 
formula_W <- as.formula(paste("W ~ ", paste(colnames(temp_data_W), collapse= "+")))
temp_data_W$W <- data$wolffia_sp[-129] # add W presence 
formula_W # view the formula 

# GLM for full model 
glm_W_scaled_out <- glm(formula_W, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_scaled_out)

# null model 
glm_W_scaled_out_null <- glm(W ~ 1, family=binomial, data=temp_data_W, na.action = "na.fail")
summary(glm_W_scaled_out_null)

###############
# FP presence # 
# Full & Null #
###############
# Build full and null models
temp_data_FPpres <- dataENV_scaled_out
temp_data_FPpres$nearest_any_FP <- NULL
temp_data_FPpres$ALK_avg <- NULL   # remove ALK_avg 
temp_data_FPpres$depth_max_m <- NULL   # remove depth_max_m 
temp_data_FPpres$dist_waterfowl <- NULL   # remove dist_waterfowl 


# make formula 
formula_FPpres <- as.formula(paste("FPpres ~ ", paste(colnames(temp_data_FPpres), collapse= "+")))
temp_data_FPpres$FPpres <- as.numeric(data$FP_presence)[-129] # add FP ppresence 
formula_FPpres # view the formula 

# GLM for full model 
glm_FPpres_scaled_out <- glm(formula_FPpres, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_scaled_out)

# null model 
glm_FPpres_scaled_out_null <- glm(FPpres ~ 1, family=binomial, data=temp_data_FPpres, na.action = "na.fail")
summary(glm_FPpres_scaled_out_null)

###############
# FP richness # 
# Full & Null #
###############
# Build full and null models
temp_data_FPrich <- dataENV_scaled_out
temp_data_FPrich$nearest_any_FP <- NULL   # remove nearest LM,SP,or W
temp_data_FPrich$ALK_avg <- NULL   # remove ALK_avg 
temp_data_FPrich$depth_max_m <- NULL   # remove depth_max_m 
temp_data_FPrich$dist_waterfowl <- NULL   # remove dist_waterfowl 

# make formula 
formula_FPrich <- as.formula(paste("FPrich ~ ", paste(colnames(temp_data_FPrich), collapse= "+")))
temp_data_FPrich$FPrich <- data$FP_species_richness[-129] # add FP ppresence 
formula_FPrich # view the formula 

# GLM for full model 
glm_FPrich_scaled_out <- glm(formula_FPrich, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_scaled_out)

# null model 
glm_FPrich_scaled_out_null <- glm(FPrich ~ 1, family=poisson, data=temp_data_FPrich, na.action = "na.fail")
summary(glm_FPrich_scaled_out_null)




##################################
# Running all models in parallel #
# pdredge()                      #
##################################
# create a cluster 
library(doSNOW)
library(MuMIn)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_LM")

# run the model in parallel
all_glm_LM_scaled_out_reduc <- pdredge(glm_LM_scaled_out, cluster=clust)
numb_models <- nrow(all_glm_LM_scaled_out_reduc)
write.csv(all_glm_LM_scaled_out_reduc[1:numb_models],file="all_glm_LM_scaled_out_reduc5.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_SP")

# run the model in parallel
all_glm_SP_scaled_out_reduc <- pdredge(glm_SP_scaled_out, cluster=clust)
numb_models <- nrow(all_glm_SP_scaled_out_reduc)
write.csv(all_glm_SP_scaled_out_reduc[1:numb_models],file="all_glm_SP_scaled_out_reduc5.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")

# stop the cluster 
stopCluster(clust)



# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_W")

# run the model in parallel
all_glm_W_scaled_out_reduc <- pdredge(glm_W_scaled_out, cluster=clust)
numb_models <- nrow(all_glm_W_scaled_out_reduc)
write.csv(all_glm_W_scaled_out_reduc[1:numb_models],file="all_glm_W_scaled_out_reduc5.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPpres")

# run the model in parallel
all_glm_FPpres_scaled_out_reduc <- pdredge(glm_FPpres_scaled_out, cluster=clust)
numb_models <- nrow(all_glm_FPpres_scaled_out_reduc)
write.csv(all_glm_FPpres_scaled_out_reduc[1:numb_models],file="all_glm_FPpres_scaled_out_reduc5.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPrich")

# run the model in parallel
all_glm_FPrich_scaled_out_reduc <- pdredge(glm_FPrich_scaled_out, cluster=clust)
numb_models <- nrow(all_glm_FPrich_scaled_out_reduc)
write.csv(all_glm_FPrich_scaled_out_reduc[1:numb_models],file="all_glm_FPrich_scaled_out_reduc5.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")

# stop the cluster 
stopCluster(clust)



###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM_scaled_out_reduc <- model.avg(all_glm_LM_scaled_out_reduc, subset = delta < 2)
avg_SP_scaled_out_reduc <- model.avg(all_glm_SP_scaled_out_reduc, subset = delta < 2)
avg_W_scaled_out_reduc <- model.avg(all_glm_W_scaled_out_reduc, subset = delta < 2)
avg_FPpres_scaled_out_reduc <- model.avg(all_glm_FPpres_scaled_out_reduc, subset = delta < 2)
avg_FPrich_scaled_out_reduc <- model.avg(all_glm_FPrich_scaled_out_reduc, subset = delta < 2)

# Re-do the model averaging based on explicity specifying a model list 
# this is so I can use predict() with the model average object 
model_list_LM <- get.models(all_glm_LM_scaled_out_reduc, subset = delta < 2) # specify the list of models whose delta AIC < 2
model_list_SP <- get.models(all_glm_SP_scaled_out_reduc, subset = delta < 2)
model_list_W <- get.models(all_glm_W_scaled_out_reduc, subset = delta < 2)
model_list_FPpress <- get.models(all_glm_FPpres_scaled_out_reduc, subset = delta < 2)
model_list_FPrich <- get.models(all_glm_FPrich_scaled_out_reduc, subset = delta < 2)

avg_LM_scaled_out_reduc <- model.avg(model_list_LM) # use that list to do the model averaging 
avg_SP_scaled_out_reduc <- model.avg(model_list_SP)
avg_W_scaled_out_reduc <- model.avg(model_list_W)
avg_FPpres_scaled_out_reduc <- model.avg(model_list_FPpress)
avg_FPrich_scaled_out_reduc <- model.avg(model_list_FPrich)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM_scaled_out_reduc)
summary(avg_SP_scaled_out_reduc)
summary(avg_W_scaled_out_reduc)
summary(avg_FPpres_scaled_out_reduc)
summary(avg_FPrich_scaled_out_reduc)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM_scaled_out_reduc)
importance(avg_SP_scaled_out_reduc)
importance(avg_W_scaled_out_reduc)
importance(avg_FPpres_scaled_out_reduc)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM_scaled_out_reduc)
confint(avg_SP_scaled_out_reduc)
confint(avg_W_scaled_out_reduc)
confint(avg_FPpres_scaled_out_reduc)

##############################
# number of component models #
##############################
nrow(summary(avg_LM_scaled_out_reduc)[1]$summary)
nrow(summary(avg_SP_scaled_out_reduc)[1]$summary)
nrow(summary(avg_W_scaled_out_reduc)[1]$summary)
nrow(summary(avg_FPpres_scaled_out_reduc)[1]$summary)
nrow(summary(avg_FPrich_scaled_out_reduc)[1]$summary)

##############
# Best model #  
##############
library(MuMIn)

# assign the best model to an object 
best_glm_LM_scaled_out_reduc <- get.models(all_glm_LM_scaled_out_reduc, 1)[1]
best_glm_SP_scaled_out_reduc <- get.models(all_glm_SP_scaled_out_reduc, 1)[1]
best_glm_W_scaled_out_reduc <- get.models(all_glm_W_scaled_out_reduc, 1)[1]
best_glm_FPpres_scaled_out_reduc <- get.models(all_glm_FPpres_scaled_out_reduc, 1)[1]
best_glm_FPrich_scaled_out_reduc <- get.models(all_glm_FPrich_scaled_out_reduc, 1)[1]

# but we actually want this to be the fitted model 
# I deleted a "+ 1" from the end of each of these formulas - not sure why it was there (MJM 8/27/2014) 
best_glm_LM_scaled_out_reduc <- glm(LM ~ COND_avg + latitude + secchi_avg + TOTP_avg, 
                         family = binomial, 
                         data = temp_data_LM, 
                         na.action = "na.fail")

best_glm_SP_scaled_out_reduc <- glm(SP ~ COND_avg + secchi_avg, 
                         family = binomial, 
                         data = temp_data_SP, 
                         na.action = "na.fail")

best_glm_W_scaled_out_reduc <- glm(W ~ boatlaunch + COND_avg + secchi_avg + waterbodies_10km, 
                        family = binomial, 
                        data = temp_data_W, na.action = "na.fail")

best_glm_FPpres_scaled_out_reduc <- glm(FPpres ~ COND_avg  + nearest_SP + secchi_avg + TOTP_avg,
                             family = binomial, 
                             data = temp_data_FPpres, 
                             na.action = "na.fail")

best_glm_FPrich_scaled_out_reduc <- glm(FPrich ~  COND_avg + latitude + nearest_SP + secchi_avg + TOTP_avg,
                             family = poisson, 
                             data = temp_data_FPrich, 
                             na.action = "na.fail")

######################
# pseudo-R-squared   #
# Variance explained #
# Full model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(best_glm_LM_scaled_out_reduc, null = glm_LM_scaled_out_null)
r.squaredLR(best_glm_SP_scaled_out_reduc, null = glm_SP_scaled_out_null)
r.squaredLR(best_glm_W_scaled_out_reduc, null = glm_W_scaled_out_null)
r.squaredLR(best_glm_FPpres_scaled_out_reduc, null = glm_FPpres_scaled_out_null)

######################
# pseudo-R-squared   #
# Variance explained #
# Best model         #
######################
# statistic based on improvment from null model - compares likelihood of both 
# adj.r.squared is base on Nagelkerke 1991
library(MuMIn)

r.squaredLR(glm_LM_scaled_out, null = glm_LM_scaled_out_null)
r.squaredLR(glm_SP_scaled_out, null = glm_SP_scaled_out_null)
r.squaredLR(glm_W_scaled_out, null = glm_W_scaled_out_null)
r.squaredLR(glm_FPpres_scaled_out, null = glm_FPpres_scaled_out_null)

###################
# Model residuals # 
# Best model      #
################### 
# get model residuals from the single best model 

best_glm_LM_scaled_out_reduc5_resid <- resid(best_glm_LM_scaled_out_reduc)
best_glm_LM_scaled_out_reduc5_resid <- as.vector(best_glm_LM_scaled_out_reduc5_resid)

best_glm_SP_scaled_out_reduc5_resid <- resid(best_glm_SP_scaled_out_reduc)
best_glm_SP_scaled_out_reduc5_resid <- as.vector(best_glm_SP_scaled_out_reduc5_resid)

best_glm_W_scaled_out_reduc5_resid <- resid(best_glm_W_scaled_out_reduc)
best_glm_W_scaled_out_reduc5_resid <- as.vector(best_glm_W_scaled_out_reduc5_resid)

best_glm_FPpres_scaled_out_reduc5_resid <- resid(best_glm_FPpres_scaled_out_reduc)
best_glm_FPpres_scaled_out_reduc5_resid <- as.vector(best_glm_FPpres_scaled_out_reduc5_resid)

############### 
# Save things # 
############### 
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled_out - reduced5.RData")
