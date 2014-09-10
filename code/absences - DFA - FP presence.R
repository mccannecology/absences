##################################
# Discriminant Function Analysis #
# CT Floating plant survey data  #
##################################

colnames(data)

################################
# Linear Discriminant Analysis #
################################

# organize your data 
colnames(dataENV_trans)
temp_data_FPpres <- dataENV_trans
temp_data_FPpres$nearest_LM <- NULL  # remove nearest LM
temp_data_FPpres$nearest_SP <- NULL   # remove nearest SP
temp_data_FPpres$nearest_W <- NULL   # remove nearest W
colnames(temp_data_FPpres)

# re-order the columns so FPpres (your group) is first
temp_data_FPpres <- temp_data_FPpres[c(14,seq(1,13,1))]

# run the linear DFA 
# specify the variables explicitly this time (I know. It's ugly. But it works.)
dfa_FPpres <- lda(temp_data_FPpres$FPpres ~ temp_data_FPpres$surfacearea_ha + temp_data_FPpres$shoreline_development + 
                    temp_data_FPpres$TOTP_avg + temp_data_FPpres$PH_avg + temp_data_FPpres$COND_avg + 
                    temp_data_FPpres$secchi_avg + temp_data_FPpres$waterbodies_1km + temp_data_FPpres$waterbodies_10km + 
                    temp_data_FPpres$dist_waterfowl + temp_data_FPpres$nearest_LMSPW + temp_data_FPpres$latitude + 
                    temp_data_FPpres$longitude + temp_data_FPpres$boatlaunch)
dfa_FPpres# show results 

# Scatter plot using the 1st two discriminant dimensions 
plot(dfa_FPpres) # fit from lda


# export to .csv 
write.csv(temp_data_FPpres,"temp_data_FPpres.csv",row.names=FALSE)


##################################
# re-run: jack-knifed prediction #
##################################
dfa_FPpres <- lda(temp_data_FPpres$FPpres ~ temp_data_FPpres$surfacearea_ha + temp_data_FPpres$shoreline_development + 
                    temp_data_FPpres$TOTP_avg + temp_data_FPpres$PH_avg + temp_data_FPpres$COND_avg + 
                    temp_data_FPpres$secchi_avg + temp_data_FPpres$waterbodies_1km + temp_data_FPpres$waterbodies_10km + 
                    temp_data_FPpres$dist_waterfowl + temp_data_FPpres$nearest_LMSPW + temp_data_FPpres$latitude + 
                    temp_data_FPpres$longitude + temp_data_FPpres$boatlaunch,
                  na.action="na.omit", 
                  CV=TRUE)
dfa_FPpres # show results

# Assess the accuracy of the prediction
# percent correct for each category of G
dfa_FPpres_correct <- table(temp_data_FPpres$FPpres, dfa_FPpres$class)
diag(prop.table(dfa_FPpres_correct, 1))

# total percent correct
sum(diag(prop.table(dfa_FPpres_correct)))




