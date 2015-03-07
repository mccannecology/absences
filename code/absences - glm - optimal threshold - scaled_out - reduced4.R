library(PresenceAbsence)
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced4.RData")
# or 
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - avg glm - scaled_out - reduced4 - opt_threshold.RData")

#########################
# Build new data frames #
#########################
data_LM_threshold <- data.frame(seq(1,nrow(temp_data_LM),1),temp_data_LM$LM,predict(avg_LM_scaled_out_reduc, temp_data_LM, type='response'))
colnames(data_LM_threshold) <- c("site","obsv","pred")

data_SP_threshold <- data.frame(seq(1,nrow(temp_data_SP),1),temp_data_SP$SP,predict(avg_SP_scaled_out_reduc, temp_data_SP, type='response'))
colnames(data_SP_threshold) <- c("site","obsv","pred")

data_W_threshold <- data.frame(seq(1,nrow(temp_data_W),1),temp_data_W$W,predict(avg_W_scaled_out_reduc, temp_data_W, type='response'))
colnames(data_W_threshold) <- c("site","obsv","pred")

data_FPpres_threshold <- data.frame(seq(1,nrow(temp_data_FPpres),1),temp_data_FPpres$FPpres,predict(avg_FPpres_scaled_out_reduc, temp_data_FPpres, type='response'))
colnames(data_FPpres_threshold) <- c("site","obsv","pred")

########################## 
# Find optimal threshold # 
##########################
optimal.thresholds(data_LM_threshold) # optimal thresholds for all methods 
LM_threh <- as.numeric(optimal.thresholds(data_LM_threshold,opt.methods=2)[2]) # threshold where sensitivity=specificity 
error.threshold.plot(data_LM_threshold,main="LM")

optimal.thresholds(data_SP_threshold)
SP_threh <- as.numeric(optimal.thresholds(data_SP_threshold,opt.methods=2)[2]) # threshold where sensitivity=specificity 
error.threshold.plot(data_SP_threshold,main="SP")

optimal.thresholds(data_W_threshold)
W_threh <- as.numeric(optimal.thresholds(data_W_threshold,opt.methods=2)[2]) # threshold where sensitivity=specificity 
error.threshold.plot(data_W_threshold,main="W")

optimal.thresholds(data_FPpres_threshold)
FPpres_threh <- as.numeric(optimal.thresholds(data_FPpres_threshold,opt.methods=2)[2]) # threshold where sensitivity=specificity 
error.threshold.plot(data_FPpres_threshold,main="FPpres")

############################ 
# Re-do confusion matrices #
############################
LM_ConMat <- cmx(data_LM_threshold, threshold=LM_threh)
SP_ConMat <- cmx(data_SP_threshold, threshold=SP_threh)
W_ConMat <- cmx(data_W_threshold, threshold=W_threh)
FPpres_ConMat <- cmx(data_FPpres_threshold, threshold=FPpres_threh)

###################
# calculate Kappa #
###################
Kappa(LM_ConMat)
Kappa(SP_ConMat)
Kappa(W_ConMat)
Kappa(FPpres_ConMat)

##############
# save stuff #
##############
save.image("C:/Users/Mike/Desktop/Dropbox/absences/workspace - avg glm - scaled_out - reduced4 - opt_threshold.RData")
