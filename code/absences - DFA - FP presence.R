##################################
# Discriminant Function Analysis #
# CT Floating plant survey data  #
##################################
library(MASS)

colnames(data)

#######################################
# Set-up data frames for each species #
#######################################

# organize your data 
colnames(dataENV_trans)
DFA_data_FPpres <- dataENV_trans
DFA_data_FPpres$nearest_LMSPW <- NULL  # remove nearest LMSPW - keep in nearest LM, SP, and W individually

DFA_data_LMpres <- dataENV_trans
DFA_data_LMpres$nearest_SP <- NULL  
DFA_data_LMpres$nearest_W <- NULL  
DFA_data_LMpres$nearest_LMSPW <- NULL  

DFA_data_SPpres <- dataENV_trans
DFA_data_SPpres$nearest_LM <- NULL  
DFA_data_SPpres$nearest_W <- NULL  
DFA_data_SPpres$nearest_LMSPW <- NULL  

DFA_data_Wpres <- dataENV_trans
DFA_data_Wpres$nearest_LM <- NULL  
DFA_data_Wpres$nearest_SP <- NULL  
DFA_data_Wpres$nearest_LMSPW <- NULL  


#############################
# Add the species occurence #
############################# 
DFA_data_FPpres <- cbind(data$FP_presence,DFA_data_FPpres)
colnames(DFA_data_FPpres)[1] <- "FP_presence"

DFA_data_LMpres <- cbind(data$lemna_minor,DFA_data_LMpres)
colnames(DFA_data_LMpres)[1] <- "LM_presence"

DFA_data_SPpres <- cbind(data$spirodela_polyrhiza,DFA_data_SPpres)
colnames(DFA_data_SPpres)[1] <- "SP_presence"

DFA_data_Wpres <- cbind(data$wolffia_sp,DFA_data_Wpres)
colnames(DFA_data_Wpres)[1] <- "W_presence"


################################
# Linear Discriminant Analysis #
################################
# run the linear DFA 
# specify the variables explicitly this time (I know. It's ugly. But it works.)
DFA_FPpres <- lda(DFA_data_FPpres$FP_presence ~ DFA_data_FPpres$surfacearea_ha * DFA_data_FPpres$shoreline_development * 
                    DFA_data_FPpres$TOTP_avg * DFA_data_FPpres$PH_avg * DFA_data_FPpres$COND_avg * 
                    DFA_data_FPpres$secchi_avg * DFA_data_FPpres$waterbodies_1km * DFA_data_FPpres$waterbodies_10km * 
                    DFA_data_FPpres$dist_waterfowl * DFA_data_FPpres$nearest_LMSPW * DFA_data_FPpres$latitude * 
                    DFA_data_FPpres$longitude * DFA_data_FPpres$boatlaunch)
DFA_FPpres# show results 

# Scatter plot using the 1st two discriminant dimensions 
plot(DFA_FPpres) # fit from lda


##############################################
# Save .csv data files for DFA in STATISTICA #
############################################## 
# export to .csv 
write.csv(DFA_data_FPpres,"DFA_data_FPpres.csv",row.names=FALSE)
write.csv(DFA_data_LMpres,"DFA_data_LMpres.csv",row.names=FALSE)
write.csv(DFA_data_SPpres,"DFA_data_SPpres.csv",row.names=FALSE)
write.csv(DFA_data_Wpres,"DFA_data_Wpres.csv",row.names=FALSE)


##################################
# re-run: jack-knifed prediction #
##################################
DFA_FPpres_2 <- lda(DFA_data_FPpres$FP_presence ~ DFA_data_FPpres$surfacearea_ha + DFA_data_FPpres$shoreline_development + 
                    DFA_data_FPpres$TOTP_avg + DFA_data_FPpres$PH_avg + DFA_data_FPpres$COND_avg + 
                    DFA_data_FPpres$secchi_avg + DFA_data_FPpres$waterbodies_1km + DFA_data_FPpres$waterbodies_10km + 
                    DFA_data_FPpres$dist_waterfowl + DFA_data_FPpres$nearest_LMSPW + DFA_data_FPpres$latitude + 
                    DFA_data_FPpres$longitude + DFA_data_FPpres$boatlaunch,
                  na.action="na.omit", 
                  CV=TRUE)
DFA_FPpres_2 # show results

# Assess the accuracy of the prediction
# percent correct for each category of G
DFA_FPpres_2_correct <- table(DFA_data_FPpres$FP_presence, DFA_FPpres_2$class)
diag(prop.table(DFA_FPpres_2_correct, 1))

# total percent correct
sum(diag(prop.table(DFA_FPpres_2_correct)))

############################################
# Quadratic discrim. function              #
# Does not assume homog var-covar matrices #
############################################
qfa_FPpres <- qda(DFA_data_FPpres$FP_presence ~ DFA_data_FPpres$surfacearea_ha + DFA_data_FPpres$shoreline_development + 
                    DFA_data_FPpres$TOTP_avg + DFA_data_FPpres$PH_avg + DFA_data_FPpres$COND_avg + 
                    DFA_data_FPpres$secchi_avg + DFA_data_FPpres$waterbodies_1km + DFA_data_FPpres$waterbodies_10km + 
                    DFA_data_FPpres$dist_waterfowl + DFA_data_FPpres$nearest_LMSPW + DFA_data_FPpres$latitude + 
                    DFA_data_FPpres$longitude + DFA_data_FPpres$boatlaunch)

plot(qfa_FPpres)

