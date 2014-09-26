library(ggplot2)
library(MuMIn)

# these objects should exist already 
avg_LM_trans
avg_SP_trans
avg_W_trans
avg_FPpres_trans
avg_FPrich_trans

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# LEMNA               # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_LM_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_LM_trans_coeff$coefficient<- c(0,0.05502,2.82284,0.16145,4.02648,-3.13467,0,
                                    -1.74624,0.27869,1.53048,2.31437,0,-2.87057,0.09156)
avg_LM_trans_coeff$stand_err <- c(0,0.34597,1.71324,0.68873,1.27962,1.41236,0,
                                  0.82219,0.75429,1.17413,1.56911,0,1.01017,0.39202)
avg_LM_trans_coeff

# Re-order the variables 
avg_LM_trans_coeff$predictors <- factor(avg_LM_trans_coeff$predictors, levels=c("size","shoreline",
                                                                                    "totalP","pH","cond","secchi",
                                                                                    "nonFP","lakes_1km",
                                                                                    "lakes_10km","waterfowl","dist_LM",
                                                                                    "boatlaunch","latitude","longitude") ) 

LM_coeff <- ggplot(avg_LM_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff <- LM_coeff + geom_bar(colour="black",stat="identity")
LM_coeff <- LM_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff <- LM_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff <- LM_coeff + xlab("Predictor")
LM_coeff <- LM_coeff + ylab("Coefficient")
LM_coeff <- LM_coeff + theme_classic(base_size=18)
LM_coeff <- LM_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
LM_coeff <- LM_coeff + ggtitle("Lemna minor")
LM_coeff <- LM_coeff + geom_hline(yintercept=0)
LM_coeff
ggsave("model_avg_LM_coeff.jpg",LM_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# SPIRODELA           # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_S","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_SP_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_SP_trans_coeff$coefficient<- c(0,-0.3338,1.6884,0.7245,2.7201,-1.8308,0.1486,
                                   -0.6271,-0.4611,0.6935,0,0.3002,0,0.3115)
avg_SP_trans_coeff$stand_err <- c(0,1.0314,1.4953,1.5251,1.1053,1.3968,0.1626,
                                  0.8098,1.0935,0.9827,0,0.5230,0,0.8615)
avg_SP_trans_coeff

# Re-order the variables 
avg_SP_trans_coeff$predictors <- factor(avg_SP_trans_coeff$predictors, levels=c("size","shoreline",
                                                                                    "totalP","pH","cond","secchi",
                                                                                    "nonFP","lakes_1km",
                                                                                    "lakes_10km","waterfowl","dist_S",
                                                                                    "boatlaunch","latitude","longitude") ) 

SP_coeff <- ggplot(avg_SP_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff <- SP_coeff + geom_bar(colour="black",stat="identity")
SP_coeff <- SP_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff <- SP_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff <- SP_coeff + xlab("Predictor")
SP_coeff <- SP_coeff + ylab("Coefficient")
SP_coeff <- SP_coeff + theme_classic(base_size=18)
SP_coeff <- SP_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
SP_coeff <- SP_coeff + ggtitle("Spirodela polyrhiza")
SP_coeff <- SP_coeff + geom_hline(yintercept=0)
SP_coeff
ggsave("model_avg_SP_coeff.jpg",SP_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# WOLFFIA             # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_W","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_W_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_W_trans_coeff$coefficient<- c(0,-1.3793,2.5684,0,3.6371,-3.5349,0,
                                  -0.9653,-1.7377,0.9900,0,1.0982,-0.7299,0)
avg_W_trans_coeff$stand_err <- c(0,1.2865,1.8896,0,1.3683,1.6883,0,
                                 0.9731,1.3443,1.2513,0,0.6234,0.9990,0)
avg_W_trans_coeff


# Re-order the variables 
avg_W_trans_coeff$predictors <- factor(avg_W_trans_coeff$predictors, levels=c("size","shoreline",
                                                                                  "totalP","pH","cond","secchi",
                                                                                  "nonFP","lakes_1km",
                                                                                  "lakes_10km","waterfowl","dist_W",
                                                                                  "boatlaunch","latitude","longitude") ) 

W_coeff <- ggplot(avg_W_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff <- W_coeff + geom_bar(colour="black",stat="identity")
W_coeff <- W_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff <- W_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff <- W_coeff + xlab("Predictor")
W_coeff <- W_coeff + ylab("Coefficient")
W_coeff <- W_coeff + theme_classic(base_size=18)
W_coeff <- W_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
W_coeff <- W_coeff + ggtitle("Wolffia sp.")
W_coeff <- W_coeff + geom_hline(yintercept=0)
W_coeff
ggsave("model_avg_W_coeff.jpg",W_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# FPpresence          #
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","dist_S","dist_W","boatlaunch",
                "latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - other","Regional - other")
avg_FPpres_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPpres_trans_coeff$coefficient<- c(0,0.4706,4.5985,2.1118,4.4615,-3.3853,0.1099,
                                       -0.7878,1.0931,0.5259,4.7584,-2.2126,1.0164,0,
                                       -2.1019,1.1981)
avg_FPpres_trans_coeff$stand_err <- c(0,0.9899,1.6047,1.5236,1.3410,1.3850,0.1597,
                                      0.7577,1.1170,0.9836,1.7320,1.2664,1.5210,0,
                                      0.9791,0.8744)
avg_FPpres_trans_coeff

# Re-order the variables 
avg_FPpres_trans_coeff$predictors <- factor(avg_FPpres_trans_coeff$predictors, levels=c("size","shoreline",
                                                                                            "totalP","pH","cond","secchi",
                                                                                            "nonFP","lakes_1km",
                                                                                            "lakes_10km","waterfowl",
                                                                                            "dist_LM","dist_S","dist_W",
                                                                                            "boatlaunch","latitude","longitude") ) 

FPpres_coeff <- ggplot(avg_FPpres_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff <- FPpres_coeff + geom_bar(colour="black",stat="identity")
FPpres_coeff <- FPpres_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff <- FPpres_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff <- FPpres_coeff + xlab("Predictor")
FPpres_coeff <- FPpres_coeff + ylab("Coefficient")
FPpres_coeff <- FPpres_coeff + theme_classic(base_size=18)
FPpres_coeff <- FPpres_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_coeff <- FPpres_coeff + ggtitle("Floating plant presence")
FPpres_coeff <- FPpres_coeff + geom_hline(yintercept=0)
FPpres_coeff
ggsave("model_avg_FPpres_coeff.jpg",FPpres_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# FPrichness          #
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","dist_S","dist_W","boatlaunch",
                "latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - other","Regional - other") 
avg_FPrich_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPrich_trans_coeff$coefficient<- c(0,0,1.38166,0.73306,2.63538,-1.78146,0.05493,
                                       -0.76897,0,0.85554,2.07708,-0.88129,0.62034,0.24556,
                                       -0.98142,0.22757)
avg_FPrich_trans_coeff$stand_err <- c(0,0,0.69087,0.71023,0.54109,0.67971,0.08199,
                                      0.38458,0,0.48170,0.80892,0.64021,0.72469,0.25031,
                                      0.41976,0.41503)
avg_FPrich_trans_coeff

# Re-order the variables 
avg_FPrich_trans_coeff$predictors <- factor(avg_FPrich_trans_coeff$predictors, levels=c("size","shoreline",
                                                                                            "totalP","pH","cond","secchi",
                                                                                            "nonFP","lakes_1km",
                                                                                            "lakes_10km","waterfowl",
                                                                                            "dist_LM","dist_S","dist_W",
                                                                                            "boatlaunch","latitude","longitude") ) 

FPrich_coeff <- ggplot(avg_FPrich_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPrich_coeff <- FPrich_coeff + geom_bar(colour="black",stat="identity")
FPrich_coeff <- FPrich_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPrich_coeff <- FPrich_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPrich_coeff <- FPrich_coeff + xlab("Predictor")
FPrich_coeff <- FPrich_coeff + ylab("Coefficient")
FPrich_coeff <- FPrich_coeff + theme_classic(base_size=18)
FPrich_coeff <- FPrich_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPrich_coeff <- FPrich_coeff + ggtitle("Floating plant richness")
FPrich_coeff <- FPrich_coeff + geom_hline(yintercept=0)
FPrich_coeff
ggsave("model_avg_FPrich_coeff.jpg",FPrich_coeff,height=8,width=11)

