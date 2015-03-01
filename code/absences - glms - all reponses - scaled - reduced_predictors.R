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
temp_data_LM$TOTP_avg <- NULL   # remove totalP 
temp_data_LM$COND_avg <- NULL   # remove conductivity 
temp_data_LM$depth_max_m <- NULL   # remove depth_max_m 

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
temp_data_SP$TOTP_avg <- NULL   # remove totalP 
temp_data_SP$COND_avg <- NULL   # remove conductivity 
temp_data_SP$depth_max_m <- NULL   # remove depth_max_m 

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
temp_data_W$TOTP_avg <- NULL   # remove totalP 
temp_data_W$COND_avg <- NULL   # remove conductivity 
temp_data_W$depth_max_m <- NULL   # remove depth_max_m 

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
temp_data_FPpres$nearest_LM <- NULL  # remove nearest LM
temp_data_FPpres$nearest_SP <- NULL   # remove nearest SP
temp_data_FPpres$nearest_W <- NULL   # remove nearest W
temp_data_FPpres$TOTP_avg <- NULL   # remove totalP 
temp_data_FPpres$COND_avg <- NULL   # remove conductivity 
temp_data_FPpres$depth_max_m <- NULL   # remove depth_max_m 
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
temp_data_FPrich$TOTP_avg <- NULL   # remove totalP 
temp_data_FPrich$COND_avg <- NULL   # remove conductivity 
temp_data_FPrich$depth_max_m <- NULL   # remove depth_max_m 
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




##################################
# Running all models in parallel #
# pdredge()                      #
##################################
# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_LM")

# run the model in parallel
all_glm_LM_scaled_reduc <- pdredge(glm_LM_scaled, cluster=clust)
numb_models <- nrow(all_glm_LM_scaled_reduc)
write.csv(all_glm_LM_scaled_reduc[1:numb_models],file="all_glm_LM_scaled_reduc.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled - reduced.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_SP")

# run the model in parallel
all_glm_SP_scaled_reduc <- pdredge(glm_SP_scaled, cluster=clust)
numb_models <- nrow(all_glm_SP_scaled_reduc)
write.csv(all_glm_SP_scaled_reduc[1:numb_models],file="all_glm_SP_scaled_reduc.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled - reduced.RData")

# stop the cluster 
stopCluster(clust)



# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_W")

# run the model in parallel
all_glm_W_scaled_reduc <- pdredge(glm_W_scaled, cluster=clust)
numb_models <- nrow(all_glm_W_scaled_reduc)
write.csv(all_glm_W_scaled_reduc[1:numb_models],file="all_glm_W_scaled_reduc.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled - reduced.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPpres")

# run the model in parallel
all_glm_FPpres_scaled_reduc <- pdredge(glm_FPpres_scaled, cluster=clust)
numb_models <- nrow(all_glm_FPpres_scaled_reduc)
write.csv(all_glm_FPpres_scaled_reduc[1:numb_models],file="all_glm_FPpres_scaled_reduc.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled - reduced.RData")

# stop the cluster 
stopCluster(clust)




# create a cluster 
library(doSNOW)
clust <- makeCluster(4,"SOCK") 
clusterExport(clust, "temp_data_FPrich")

# run the model in parallel
all_glm_FPrich_scaled_reduc <- pdredge(glm_FPrich_scaled, cluster=clust)
numb_models <- nrow(all_glm_FPrich_scaled_reduc)
write.csv(all_glm_FPrich_scaled_reduc[1:numb_models],file="all_glm_FPrich_scaled_reduc.csv",na="NA")
save(list = ls(all = TRUE), file = "workspace - all glms dredge - scaled - reduced.RData")

# stop the cluster 
stopCluster(clust)


numb_models <- nrow(all_glm_LM_scaled_reduc)
write.csv(all_glm_LM_scaled_reduc[1:numb_models],file="all_glm_LM_scaled_reduc.csv",na="NA")
numb_models <- nrow(all_glm_SP_scaled_reduc)
write.csv(all_glm_SP_scaled_reduc[1:numb_models],file="all_glm_SP_scaled_reduc.csv",na="NA")
numb_models <- nrow(all_glm_W_scaled_reduc)
write.csv(all_glm_W_scaled_reduc[1:numb_models],file="all_glm_W_scaled_reduc.csv",na="NA")
numb_models <- nrow(all_glm_FPpres_scaled_reduc)
write.csv(all_glm_FPpres_scaled_reduc[1:numb_models],file="all_glm_FPpres_scaled_reduc.csv",na="NA")
numb_models <- nrow(all_glm_FPrich_scaled_reduc)
write.csv(all_glm_FPrich_scaled_reduc[1:numb_models],file="all_glm_FPrich_scaled_reduc.csv",na="NA")




###################
# Model Averaging # 
###################
library(MuMIn)

# Model average models with delta AICc < 2
avg_LM_scaled_reduc <- model.avg(all_glm_LM_scaled_reduc, subset = delta < 2)
avg_SP_scaled_reduc <- model.avg(all_glm_SP_scaled_reduc, subset = delta < 2)
avg_W_scaled_reduc <- model.avg(all_glm_W_scaled_reduc, subset = delta < 2)
avg_FPpres_scaled_reduc <- model.avg(all_glm_FPpres_scaled_reduc, subset = delta < 2)
avg_FPrich_scaled_reduc <- model.avg(all_glm_FPrich_scaled_reduc, subset = delta < 2)

# Re-do the model averaging based on explicity specifying a model list 
# this is so I can use predict() with the model average object 
model_list_LM <- get.models(all_glm_LM_scaled_reduc, subset = delta < 2) # specify the list of models whose delta AIC < 2
model_list_SP <- get.models(all_glm_SP_scaled_reduc, subset = delta < 2)
model_list_W <- get.models(all_glm_W_scaled_reduc, subset = delta < 2)
model_list_FPpress <- get.models(all_glm_FPpres_scaled_reduc, subset = delta < 2)
model_list_FPrich <- get.models(all_glm_FPrich_scaled_reduc, subset = delta < 2)

avg_LM_scaled_reduc <- model.avg(model_list_LM) # use that list to do the model averaging 
avg_SP_scaled_reduc <- model.avg(model_list_SP)
avg_W_scaled_reduc <- model.avg(model_list_W)
avg_FPpres_scaled_reduc <- model.avg(model_list_FPpress)
avg_FPrich_scaled_reduc <- model.avg(model_list_FPrich)

# get the table for with the coefficient and p-value for each variable 
summary(avg_LM_scaled_reduc)
summary(avg_SP_scaled_reduc)
summary(avg_W_scaled_reduc)
summary(avg_FPpres_scaled_reduc)
summary(avg_FPrich_scaled_reduc)

# importance 
# Sum of ‘Akaike weights’ over all models including the explanatory variable 
importance(avg_LM_scaled_reduc)
importance(avg_SP_scaled_reduc)
importance(avg_W_scaled_reduc)
importance(avg_FPpres_scaled_reduc)

# get 95% confidence intervals for estimates
# if they span 0, then the coefficient is not different than 0 
# significance (p-values) produced by summary() do not agree with 95% CI spanning 0 
confint(avg_LM_scaled_reduc)
confint(avg_SP_scaled_reduc)
confint(avg_W_scaled_reduc)
confint(avg_FPpres_scaled_reduc)


############### 
# Save things # 
############### 
#save.image("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms - dredge - scaled - reduced.RData")
