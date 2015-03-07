load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5.RData")
# or
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5 - coeff_plots.RData")

library(ggplot2)
library(MuMIn)
library(gridExtra)

# these objects should exist already 
summary(avg_LM_scaled_out_reduc)
summary(avg_SP_scaled_out_reduc)
summary(avg_W_scaled_out_reduc)
summary(avg_FPpres_scaled_out_reduc)
summary(avg_FPrich_scaled_out_reduc)

###########################################################
# Extract coefficients @ set-up data frames for plotting  #
# LM                                                      #
###########################################################
# set up the data frame that contains all predictors (this gets merged with the data from each model)
predictors <- c("size","shoreline","totalP","pH",
                                 "cond","secchi","nonFP","lakes1km",
                                 "lakes10km","distLM","boatlaunch","latitude",
                                 "longitude")

# a column for the variable type
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 

# combine 'em
coeff_df_temp <- as.data.frame(cbind(predictors,predictor_type))

# make coefficient and std. error a data frame 
avg_LM_scaled_out_reduc_coeff <- as.data.frame(avg_LM_scaled_out_reduc$avg.model[,1:2])

# I also want the p-values
avg_LM_scaled_out_reduc_coeff$pvalue <- summary(avg_LM_scaled_out_reduc)[14]$coefmat.full[,5]

# rename the variables 
names(avg_LM_scaled_out_reduc_coeff)[1] <- "coefficient"
names(avg_LM_scaled_out_reduc_coeff)[2] <- "stand_err"

# turn the row names (predictors names) into a column 
avg_LM_scaled_out_reduc_coeff$predictors <- row.names(avg_LM_scaled_out_reduc_coeff)
row.names(avg_LM_scaled_out_reduc_coeff) <- NULL

# re-name the predictors 
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "surfacearea_ha"] <- "size"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "shoreline_development"] <- "shoreline"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "nonFP_species_richness"] <- "nonFP"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "TOTP_avg"] <- "totalP"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "PH_avg"] <- "pH"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "COND_avg"] <- "cond"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "secchi_avg"] <- "secchi"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "waterbodies_1km"] <- "lakes1km"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "waterbodies_10km"] <- "lakes10km"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "nearest_LM"] <- "distLM"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "nearest_SP"] <- "distSP"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "nearest_W"] <- "distW"
avg_LM_scaled_out_reduc_coeff$predictors[avg_LM_scaled_out_reduc_coeff$predictors == "boatlaunch"] <- "boatlaunch"

# remove the intercept
avg_LM_scaled_out_reduc_coeff <- avg_LM_scaled_out_reduc_coeff[2:nrow(avg_LM_scaled_out_reduc_coeff),]

# merge the two data frames 
avg_LM_scaled_out_reduc_coeff <- merge(coeff_df_temp,avg_LM_scaled_out_reduc_coeff,by="predictors",all.x=TRUE)

# Re-order the variables 
avg_LM_scaled_out_reduc_coeff$predictors <- factor(avg_LM_scaled_out_reduc_coeff$predictors, levels=c("size","shoreline","totalP","pH",
                                                                                                      "cond","secchi","nonFP","lakes1km",
                                                                                                      "lakes10km","distLM","boatlaunch","latitude",
                                                                                                      "longitude") ) 
# add an asterisk (*) label for significant coefficients 
avg_LM_scaled_out_reduc_coeff$label <- NA
avg_LM_scaled_out_reduc_coeff$label[avg_LM_scaled_out_reduc_coeff$pvalue < 0.05] <- "*"

#######################
# Plotting - LM       # 
#######################
LM_coeff <- ggplot(avg_LM_scaled_out_reduc_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff <- LM_coeff + geom_bar(colour="black",stat="identity")
LM_coeff <- LM_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff <- LM_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff <- LM_coeff + xlab("Predictor")
LM_coeff <- LM_coeff + ylab("Coefficient")
LM_coeff <- LM_coeff + theme_classic(base_size=18)
LM_coeff <- LM_coeff + geom_text(aes(1,7,label="a)"))
LM_coeff <- LM_coeff + geom_text(data=avg_LM_scaled_out_reduc_coeff,size=8,aes(x=predictors, y=ifelse(coefficient >= 0,
                                                                                                      coefficient+stand_err+0.25,
                                                                                                      coefficient-stand_err-0.55)
                                                                               ,label=label))
LM_coeff <- LM_coeff + theme(axis.text.x = element_text(angle=45, hjust=1))
LM_coeff <- LM_coeff + geom_hline(yintercept=0)
LM_coeff
ggsave("model_avg_LM_coeff_dataENV_scaled_out_reduc5.jpg",LM_coeff,height=8,width=11)

###########################################################
# Extract coefficients @ set-up data frames for plotting  #
# SP                                                      #
###########################################################
# set up the data frame that contains all predictors (this gets merged with the data from each model)
predictors <- c("size","shoreline","totalP","pH",
                "cond","secchi","nonFP","lakes1km",
                "lakes10km","distSP","boatlaunch","latitude",
                "longitude")

# a column for the variable type
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 

# combine 'em
coeff_df_temp <- as.data.frame(cbind(predictors,predictor_type))

# make coefficient and std. error a data frame 
avg_SP_scaled_out_reduc_coeff <- as.data.frame(avg_SP_scaled_out_reduc$avg.model[,1:2])

# I also want the p-values
avg_SP_scaled_out_reduc_coeff$pvalue <- summary(avg_SP_scaled_out_reduc)[14]$coefmat.full[,5]

# rename the variables 
names(avg_SP_scaled_out_reduc_coeff)[1] <- "coefficient"
names(avg_SP_scaled_out_reduc_coeff)[2] <- "stand_err"

# turn the row names (predictors names) into a column 
avg_SP_scaled_out_reduc_coeff$predictors <- row.names(avg_SP_scaled_out_reduc_coeff)
row.names(avg_SP_scaled_out_reduc_coeff) <- NULL

# re-name the predictors 
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "surfacearea_ha"] <- "size"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "shoreline_development"] <- "shoreline"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "nonFP_species_richness"] <- "nonFP"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "TOTP_avg"] <- "totalP"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "PH_avg"] <- "pH"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "COND_avg"] <- "cond"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "secchi_avg"] <- "secchi"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "waterbodies_1km"] <- "lakes1km"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "waterbodies_10km"] <- "lakes10km"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "nearest_SP"] <- "distSP"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "nearest_SP"] <- "distSP"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "nearest_W"] <- "distW"
avg_SP_scaled_out_reduc_coeff$predictors[avg_SP_scaled_out_reduc_coeff$predictors == "boatlaunch"] <- "boatlaunch"

# remove the intercept
avg_SP_scaled_out_reduc_coeff <- avg_SP_scaled_out_reduc_coeff[2:nrow(avg_SP_scaled_out_reduc_coeff),]

# merge the two data frames 
avg_SP_scaled_out_reduc_coeff <- merge(coeff_df_temp,avg_SP_scaled_out_reduc_coeff,by="predictors",all.x=TRUE)

# Re-order the variables 
avg_SP_scaled_out_reduc_coeff$predictors <- factor(avg_SP_scaled_out_reduc_coeff$predictors, levels=c("size","shoreline","totalP","pH",
                                                                                                      "cond","secchi","nonFP","lakes1km",
                                                                                                      "lakes10km","distSP","boatlaunch","latitude",
                                                                                                      "longitude") ) 
# add an asterisk (*) label for significant coefficients 
avg_SP_scaled_out_reduc_coeff$label <- NA
avg_SP_scaled_out_reduc_coeff$label[avg_SP_scaled_out_reduc_coeff$pvalue < 0.05] <- "*"

#######################
# Plotting - SP       # 
#######################
SP_coeff <- ggplot(avg_SP_scaled_out_reduc_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff <- SP_coeff + geom_bar(colour="black",stat="identity")
SP_coeff <- SP_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff <- SP_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff <- SP_coeff + xlab("Predictor")
SP_coeff <- SP_coeff + ylab("Coefficient")
SP_coeff <- SP_coeff + theme_classic(base_size=18)
SP_coeff <- SP_coeff + geom_text(data=avg_SP_scaled_out_reduc_coeff,size=8,aes(x=predictors, y=ifelse(coefficient >= 0,
                                                                                                      coefficient+stand_err+0.25,
                                                                                                      coefficient-stand_err-0.5)
                                                                               ,label=label))
SP_coeff <- SP_coeff + theme(axis.text.x = element_text(angle=45, hjust=1))
SP_coeff <- SP_coeff + geom_text(aes(1,4,label="b)"))
SP_coeff <- SP_coeff + geom_hline(yintercept=0)
SP_coeff
ggsave("model_avg_SP_coeff_dataENV_scaled_out_reduc5.jpg",SP_coeff,height=8,width=11)


###########################################################
# Extract coefficients @ set-up data frames for plotting  #
# W                                                      #
###########################################################
# set up the data frame that contains all predictors (this gets merged with the data from each model)
predictors <- c("size","shoreline","totalP","pH",
                "cond","secchi","nonFP","lakes1km",
                "lakes10km","distW","boatlaunch","latitude",
                "longitude")

# a column for the variable type
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 

# combine 'em
coeff_df_temp <- as.data.frame(cbind(predictors,predictor_type))

# make coefficient and std. error a data frame 
avg_W_scaled_out_reduc_coeff <- as.data.frame(avg_W_scaled_out_reduc$avg.model[,1:2])

# I also want the p-values
avg_W_scaled_out_reduc_coeff$pvalue <- summary(avg_W_scaled_out_reduc)[14]$coefmat.full[,5]

# rename the variables 
names(avg_W_scaled_out_reduc_coeff)[1] <- "coefficient"
names(avg_W_scaled_out_reduc_coeff)[2] <- "stand_err"

# turn the row names (predictors names) into a column 
avg_W_scaled_out_reduc_coeff$predictors <- row.names(avg_W_scaled_out_reduc_coeff)
row.names(avg_W_scaled_out_reduc_coeff) <- NULL

# re-name the predictors 
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "surfacearea_ha"] <- "size"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "shoreline_development"] <- "shoreline"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "nonFP_species_richness"] <- "nonFP"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "TOTP_avg"] <- "totalP"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "PH_avg"] <- "pH"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "COND_avg"] <- "cond"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "secchi_avg"] <- "secchi"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "waterbodies_1km"] <- "lakes1km"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "waterbodies_10km"] <- "lakes10km"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "nearest_W"] <- "distW"
avg_W_scaled_out_reduc_coeff$predictors[avg_W_scaled_out_reduc_coeff$predictors == "boatlaunch"] <- "boatlaunch"

# remove the intercept
avg_W_scaled_out_reduc_coeff <- avg_W_scaled_out_reduc_coeff[2:nrow(avg_W_scaled_out_reduc_coeff),]

# merge the two data frames 
avg_W_scaled_out_reduc_coeff <- merge(coeff_df_temp,avg_W_scaled_out_reduc_coeff,by="predictors",all.x=TRUE)

# Re-order the variables 
avg_W_scaled_out_reduc_coeff$predictors <- factor(avg_W_scaled_out_reduc_coeff$predictors, levels=c("size","shoreline","totalP","pH",
                                                                                                      "cond","secchi","nonFP","lakes1km",
                                                                                                      "lakes10km","distW","boatlaunch","latitude",
                                                                                                      "longitude") ) 
# add an asterisk (*) label for significant coefficients 
avg_W_scaled_out_reduc_coeff$label <- NA
avg_W_scaled_out_reduc_coeff$label[avg_W_scaled_out_reduc_coeff$pvalue < 0.05] <- "*"

#######################
# Plotting - W        # 
#######################
W_coeff <- ggplot(avg_W_scaled_out_reduc_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff <- W_coeff + geom_bar(colour="black",stat="identity")
W_coeff <- W_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff <- W_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff <- W_coeff + xlab("Predictor")
W_coeff <- W_coeff + ylab("Coefficient")
W_coeff <- W_coeff + theme_classic(base_size=18)
W_coeff <- W_coeff + geom_text(data=avg_W_scaled_out_reduc_coeff,size=8,aes(x=predictors, y=ifelse(coefficient >= 0,
                                                                                                   coefficient+stand_err+0.25,
                                                                                                   coefficient-stand_err-0.5)
                                                                            ,label=label))
W_coeff <- W_coeff + theme(axis.text.x = element_text(angle=45, hjust=1))
W_coeff <- W_coeff + geom_text(aes(1,6.5,label="c)"))
W_coeff <- W_coeff + geom_hline(yintercept=0)
W_coeff
ggsave("model_avg_W_coeff_dataENV_scaled_out_reduc5.jpg",W_coeff,height=8,width=11)

###########################################################
# Extract coefficients @ set-up data frames for plotting  #
# FPpres                                                      #
###########################################################
# set up the data frame that contains all predictors (this gets merged with the data from each model)
predictors <- c("size","shoreline","totalP","pH",
                "cond","secchi","nonFP","lakes1km",
                "lakes10km","distLM","distSP","distW","boatlaunch","latitude",
                "longitude")

# a column for the variable type
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 

# combine 'em
coeff_df_temp <- as.data.frame(cbind(predictors,predictor_type))

# make coefficient and std. error a data frame 
avg_FPpres_scaled_out_reduc_coeff <- as.data.frame(avg_FPpres_scaled_out_reduc$avg.model[,1:2])

# I also want the p-values
avg_FPpres_scaled_out_reduc_coeff$pvalue <- summary(avg_FPpres_scaled_out_reduc)[14]$coefmat.full[,5]

# rename the variables 
names(avg_FPpres_scaled_out_reduc_coeff)[1] <- "coefficient"
names(avg_FPpres_scaled_out_reduc_coeff)[2] <- "stand_err"

# turn the row names (predictors names) into a column 
avg_FPpres_scaled_out_reduc_coeff$predictors <- row.names(avg_FPpres_scaled_out_reduc_coeff)
row.names(avg_FPpres_scaled_out_reduc_coeff) <- NULL

# re-name the predictors 
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "surfacearea_ha"] <- "size"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "shoreline_development"] <- "shoreline"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nonFP_species_richness"] <- "nonFP"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "TOTP_avg"] <- "totalP"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "PH_avg"] <- "pH"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "COND_avg"] <- "cond"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "secchi_avg"] <- "secchi"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "waterbodies_1km"] <- "lakes1km"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "waterbodies_10km"] <- "lakes10km"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nearest_FPpres"] <- "distFPpres"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nearest_FPpres"] <- "distFPpres"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nearest_LM"] <- "distLM"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nearest_SP"] <- "distSP"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "nearest_W"] <- "distW"
avg_FPpres_scaled_out_reduc_coeff$predictors[avg_FPpres_scaled_out_reduc_coeff$predictors == "boatlaunch"] <- "boatlaunch"

# remove the intercept
avg_FPpres_scaled_out_reduc_coeff <- avg_FPpres_scaled_out_reduc_coeff[2:nrow(avg_FPpres_scaled_out_reduc_coeff),]

# merge the two data frames 
avg_FPpres_scaled_out_reduc_coeff <- merge(coeff_df_temp,avg_FPpres_scaled_out_reduc_coeff,by="predictors",all.x=TRUE)

# Re-order the variables 
avg_FPpres_scaled_out_reduc_coeff$predictors <- factor(avg_FPpres_scaled_out_reduc_coeff$predictors, levels=c("size","shoreline","totalP","pH",
                                                                                                      "cond","secchi","nonFP","lakes1km",
                                                                                                      "lakes10km","distLM","distSP","distW",
                                                                                                      "boatlaunch","latitude",
                                                                                                      "longitude") ) 
# add an asterisk (*) label for significant coefficients 
avg_FPpres_scaled_out_reduc_coeff$label <- NA
avg_FPpres_scaled_out_reduc_coeff$label[avg_FPpres_scaled_out_reduc_coeff$pvalue < 0.05] <- "*"

#######################
# Plotting - FPpres   # 
#######################
FPpres_coeff <- ggplot(avg_FPpres_scaled_out_reduc_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff <- FPpres_coeff + geom_bar(colour="black",stat="identity")
FPpres_coeff <- FPpres_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff <- FPpres_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff <- FPpres_coeff + xlab("Predictor")
FPpres_coeff <- FPpres_coeff + ylab("Coefficient")
FPpres_coeff <- FPpres_coeff + theme_classic(base_size=18)
FPpres_coeff <- FPpres_coeff + geom_text(data=avg_FPpres_scaled_out_reduc_coeff,size=8,aes(x=predictors, y=ifelse(coefficient >= 0,
                                                                                                                  coefficient+stand_err+0.25,
                                                                                                                  coefficient-stand_err-0.5)
                                                                                           ,label=label))
FPpres_coeff <- FPpres_coeff + theme(axis.text.x = element_text(angle=45, hjust=1))
FPpres_coeff <- FPpres_coeff + geom_text(aes(1,10,label="d)"))
FPpres_coeff <- FPpres_coeff + geom_hline(yintercept=0)
FPpres_coeff
ggsave("model_avg_FPpres_coeff_dataENV_scaled_out_reduc5.jpg",FPpres_coeff,height=8,width=11)

###########################################################
# Extract coefficients @ set-up data frames for plotting  #
# FPrich                                                  #
###########################################################
# set up the data frame that contains all predictors (this gets merged with the data from each model)
predictors <- c("size","shoreline","totalP","pH",
                "cond","secchi","nonFP","lakes1km",
                "lakes10km","distLM","distSP","distW","boatlaunch","latitude",
                "longitude")

# a column for the variable type
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 

# combine 'em
coeff_df_temp <- as.data.frame(cbind(predictors,predictor_type))

# make coefficient and std. error a data frame 
avg_FPrich_scaled_out_reduc_coeff <- as.data.frame(avg_FPrich_scaled_out_reduc$avg.model[,1:2])

# I also want the p-values
avg_FPrich_scaled_out_reduc_coeff$pvalue <- summary(avg_FPrich_scaled_out_reduc)[14]$coefmat.full[,5]

# rename the variables 
names(avg_FPrich_scaled_out_reduc_coeff)[1] <- "coefficient"
names(avg_FPrich_scaled_out_reduc_coeff)[2] <- "stand_err"

# turn the row names (predictors names) into a column 
avg_FPrich_scaled_out_reduc_coeff$predictors <- row.names(avg_FPrich_scaled_out_reduc_coeff)
row.names(avg_FPrich_scaled_out_reduc_coeff) <- NULL

# re-name the predictors 
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "surfacearea_ha"] <- "size"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "shoreline_development"] <- "shoreline"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nonFP_species_richness"] <- "nonFP"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "TOTP_avg"] <- "totalP"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "PH_avg"] <- "pH"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "COND_avg"] <- "cond"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "secchi_avg"] <- "secchi"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "waterbodies_1km"] <- "lakes1km"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "waterbodies_10km"] <- "lakes10km"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nearest_FPrich"] <- "distFPrich"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nearest_FPrich"] <- "distFPrich"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nearest_LM"] <- "distLM"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nearest_SP"] <- "distSP"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "nearest_W"] <- "distW"
avg_FPrich_scaled_out_reduc_coeff$predictors[avg_FPrich_scaled_out_reduc_coeff$predictors == "boatlaunch"] <- "boatlaunch"

# remove the intercept
avg_FPrich_scaled_out_reduc_coeff <- avg_FPrich_scaled_out_reduc_coeff[2:nrow(avg_FPrich_scaled_out_reduc_coeff),]

# merge the two data frames 
avg_FPrich_scaled_out_reduc_coeff <- merge(coeff_df_temp,avg_FPrich_scaled_out_reduc_coeff,by="predictors",all.x=TRUE)

# Re-order the variables 
avg_FPrich_scaled_out_reduc_coeff$predictors <- factor(avg_FPrich_scaled_out_reduc_coeff$predictors, levels=c("size","shoreline","totalP","pH",
                                                                                                              "cond","secchi","nonFP","lakes1km",
                                                                                                              "lakes10km","distLM","distSP","distW",
                                                                                                              "boatlaunch","latitude",
                                                                                                              "longitude") ) 
# add an asterisk (*) label for significant coefficients 
avg_FPrich_scaled_out_reduc_coeff$label <- NA
avg_FPrich_scaled_out_reduc_coeff$label[avg_FPrich_scaled_out_reduc_coeff$pvalue < 0.05] <- "*"

#######################
# Plotting - FPrich   # 
#######################
FPrich_coeff <- ggplot(avg_FPrich_scaled_out_reduc_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPrich_coeff <- FPrich_coeff + geom_bar(colour="black",stat="identity")
FPrich_coeff <- FPrich_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPrich_coeff <- FPrich_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPrich_coeff <- FPrich_coeff + xlab("Predictor")
FPrich_coeff <- FPrich_coeff + ylab("Coefficient")
FPrich_coeff <- FPrich_coeff + theme_classic(base_size=18)
FPrich_coeff <- FPrich_coeff + geom_text(data=avg_FPrich_scaled_out_reduc_coeff,size=8,aes(x=predictors, y=ifelse(coefficient >= 0,
                                                                                                                  coefficient+stand_err+0.25,
                                                                                                                  coefficient-stand_err-0.5)
                                                                                           ,label=label))
FPrich_coeff <- FPrich_coeff + theme(axis.text.x = element_text(angle=45, hjust=1))
FPrich_coeff <- FPrich_coeff + geom_hline(yintercept=0)
FPrich_coeff
ggsave("model_avg_FPrich_coeff_dataENV_scaled_out_reduc5.jpg",FPrich_coeff,height=8,width=11)









############################
# Combining multiple plots # 
############################
LM_coeff <- LM_coeff + xlab("")
SP_coeff <- SP_coeff + xlab("")

combined_coeff <- arrangeGrob(LM_coeff,SP_coeff,W_coeff,FPpres_coeff)
combined_coeff
ggsave("model_avg_combined_coeff_dataENV_scaled_out_reduc5.jpg",combined_coeff,height=8,width=16)


##############
# Save stuff #
##############
save.image("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled_out - reduced5 - coeff_plots.RData")
