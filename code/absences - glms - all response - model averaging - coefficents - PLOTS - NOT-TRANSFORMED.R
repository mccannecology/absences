library(ggplot2)
library(MuMIn)
library(gridExtra)

# these objects should exist already 
avg_LM
avg_SP
avg_W
avg_FPpres
avg_FPrich

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# LEMNA               # 
#######################
summary(avg_LM)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes_1km",
                "lakes_10km","waterfowl","dist_LM","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_LM_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_LM_coeff$coefficient<- c(-1.828e-04, 0.000000, -3.574e-02, 0.000000, 0.000000,
                             6.072e-03, 4.579e-03, -7.738e-01, 0.000000, 4.764e-03,
                             0.000000, 4.434e-05, 0.000000, 0.000000, -2.794e+00,
                             -9.299e-02)
avg_LM_coeff$stand_err <- c(1.756e-03, 0.000000, 6.604e-02, 0.000000, 0.000000,
                            2.606e-03, 6.010e-03, 2.906e-01, 0.000000, 4.219e-02,
                            0.000000, 2.003e-05, 0.000000, 0.000000, 9.107e-01,
                            0.000000)
avg_LM_coeff

# Re-order the variables 
avg_LM_coeff$predictors <- factor(avg_LM_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                "cond","alk","secchi","nonFP","lakes_1km",
                                                                                "lakes_10km","waterfowl","dist_LM","boatlaunch","latitude",
                                                                                "longitude") ) 

LM_coeff <- ggplot(avg_LM_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff <- LM_coeff + geom_bar(colour="black",stat="identity")
LM_coeff <- LM_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff <- LM_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff <- LM_coeff + xlab("Predictor")
LM_coeff <- LM_coeff + ylab("Coefficient")
LM_coeff <- LM_coeff + theme_classic(base_size=18)
#LM_coeff <- LM_coeff + annotate("text", label = "*", x=4, y=4.85, size=8) 
#LM_coeff <- LM_coeff + annotate("text", label = "*", x=12, y=4.35, size=8) 
#LM_coeff <- LM_coeff + annotate("text", label = "*", x=15, y=-4, size=8) 
LM_coeff <- LM_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
LM_coeff <- LM_coeff + ggtitle("Lemna minor")
LM_coeff <- LM_coeff + geom_hline(yintercept=0)
LM_coeff
ggsave("model_avg_LM_raw_coeff.jpg",LM_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# SPIRODELA           # 
#######################
summary(avg_SP)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes_1km",
                "lakes_10km","waterfowl","dist_SP","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_SP_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_SP_coeff$coefficient <- c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
                             2.213e-03, 8.948e-03, -4.114e-01, 1.891e-03, 0.000000,
                             0.000000, 2.975e-05, 2.932e-06, 4.882e-02, -2.404e-02,
                             -9.249e-03)
avg_SP_coeff$stand_err <- c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
                            2.919e-03, 6.886e-03, 2.781e-01, 1.106e-02, 0.000000,
                            0.000000, 2.206e-05, 1.378e-05, 2.245e-01, 2.250e-01, 
                            1.158e-01)
avg_SP_coeff

# Re-order the variables 
avg_SP_coeff$predictors <- factor(avg_SP_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                "cond","alk","secchi","nonFP","lakes_1km",
                                                                                "lakes_10km","waterfowl","dist_SP","boatlaunch","latitude",
                                                                                "longitude") ) 

SP_coeff <- ggplot(avg_SP_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff <- SP_coeff + geom_bar(colour="black",stat="identity")
SP_coeff <- SP_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff <- SP_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff <- SP_coeff + xlab("Predictor")
SP_coeff <- SP_coeff + ylab("Coefficient")
SP_coeff <- SP_coeff + theme_classic(base_size=18)
#SP_coeff <- SP_coeff + annotate("text", label = "*", x=7, y=5.5, size=8) # signif. cond
SP_coeff <- SP_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
SP_coeff <- SP_coeff + ggtitle("Spirodela polyrhiza")
SP_coeff <- SP_coeff + geom_hline(yintercept=0)
SP_coeff
ggsave("model_avg_SP_raw_coeff.jpg",SP_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# WOLFFIA             # 
#######################
summary(avg_W)


predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes_1km",
                "lakes_10km","waterfowl","dist_W","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_W_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_W_coeff$coefficient<- c(8.792e-03, -1.380e-01, -3.976e-01, 0.000000, 0.000000,
                            8.239e-03, 1.213e-04, -9.438e-02, 0.000000, 0.000000,
                            -1.524e-02, 1.485e-05, 3.764e-07, 1.355e+00, -9.717e-01,  
                            0.000000)
avg_W_coeff$stand_err <- c(9.974e-03, 4.322e-01, 1.811e-01, 0.000000, 0.000000,
                           2.767e-03, 1.242e-03, 2.726e-01, 0.000000, 0.000000,
                           1.550e-02, 2.384e-05, 5.474e-06, 7.607e-01, 1.203e+00,
                           0.000000)
avg_W_coeff


# Re-order the variables 
avg_W_coeff$predictors <- factor(avg_W_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                              "cond","alk","secchi","nonFP","lakes_1km",
                                                                              "lakes_10km","waterfowl","dist_W","boatlaunch","latitude",
                                                                              "longitude") ) 

W_coeff <- ggplot(avg_W_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff <- W_coeff + geom_bar(colour="black",stat="identity")
W_coeff <- W_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff <- W_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff <- W_coeff + xlab("Predictor")
W_coeff <- W_coeff + ylab("Coefficient")
W_coeff <- W_coeff + theme_classic(base_size=18)
#W_coeff <- W_coeff + annotate("text", label = "*", x=6, y=5.7, size=8) # signif. cond
W_coeff <- W_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
W_coeff <- W_coeff + ggtitle("Wolffia sp.")
W_coeff <- W_coeff + geom_hline(yintercept=0)
W_coeff
ggsave("model_avg_W_raw_coeff.jpg",W_coeff,height=8,width=11)

#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# FPpresence          #
#######################
summary(avg_FPpres)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes_1km",
                "lakes_10km","waterfowl","dist_LM","dist_SP","dist_W",
                "boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 
avg_FPpres_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPpres_coeff$coefficient<- c(0.000000, 0.000000, -1.099e-02, 1.382e+01, 0.000000,
                                 3.731e-03, 2.338e-02, -7.781e-01, 6.684e-03, 0.000000,
                                 2.413e-04, 3.065e-05, 0.000000, 1.888e-05, 0.000000,
                                 0.000000, -1.036e+00, 0.000000)
avg_FPpres_coeff$stand_err <- c(0.000000, 0.000000, 4.113e-02, 4.720e+00, 0.000000,
                                3.503e-03, 1.006e-02, 2.740e-01, 1.828e-02, 0.000000,
                                2.333e-03, 2.477e-05, 0.000000, 3.274e-05, 0.000000,
                                0.000000, 1.061e+00, 0.000000)
                                
avg_FPpres_coeff

# Re-order the variables 
avg_FPpres_coeff$predictors <- factor(avg_FPpres_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                        "cond","alk","secchi","nonFP","lakes_1km",
                                                                                        "lakes_10km","waterfowl","dist_LM","dist_SP","dist_W",
                                                                                        "boatlaunch","latitude","longitude") ) 

FPpres_coeff <- ggplot(avg_FPpres_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff <- FPpres_coeff + geom_bar(colour="black",stat="identity")
FPpres_coeff <- FPpres_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff <- FPpres_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff <- FPpres_coeff + xlab("Predictor")
FPpres_coeff <- FPpres_coeff + ylab("Coefficient")
FPpres_coeff <- FPpres_coeff + theme_classic(base_size=18)
FPpres_coeff <- FPpres_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_coeff <- FPpres_coeff + geom_hline(yintercept=0)
#FPpres_coeff <- FPpres_coeff + annotate("text", label = "*", x=7, y=7, size=8) 
#FPpres_coeff <- FPpres_coeff + annotate("text", label = "*", x=4, y=5.95, size=8)  
FPpres_coeff
ggsave("model_avg_FPpres_raw_coeff.jpg",FPpres_coeff,height=8,width=11)

# re-save without a legend 
temp <- FPpres_coeff
temp <- temp + theme(legend.position="none")
temp <- temp + ggtitle("")
temp 
ggsave("model_avg_FPpres_raw_coeff-no_legend.jpg",temp,height=8,width=11)


#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# FPrichness          #
#######################
summary(avg_FPrich)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes_1km",
                "lakes_10km","waterfowl","dist_LM","dist_SP","dist_W",
                "boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 
avg_FPrich_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPrich_coeff$coefficient<- c()
avg_FPrich_coeff$stand_err <- c()
avg_FPrich_coeff

# Re-order the variables 
avg_FPrich_coeff$predictors <- factor(avg_FPrich_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                        "cond","alk","secchi","nonFP","lakes_1km",
                                                                                        "lakes_10km","waterfowl","dist_LM","dist_SP","dist_W",
                                                                                        "boatlaunch","latitude","longitude") ) 

FPrich_coeff <- ggplot(avg_FPrich_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPrich_coeff <- FPrich_coeff + geom_bar(colour="black",stat="identity")
FPrich_coeff <- FPrich_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPrich_coeff <- FPrich_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPrich_coeff <- FPrich_coeff + xlab("Predictor")
FPrich_coeff <- FPrich_coeff + ylab("Coefficient")
FPrich_coeff <- FPrich_coeff + theme_classic(base_size=18)
FPrich_coeff <- FPrich_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
#FPrich_coeff <- FPrich_coeff + ggtitle("Floating plant richness")
FPrich_coeff <- FPrich_coeff + geom_hline(yintercept=0)
#FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=6, y=2.95, size=8) 
#FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=3, y=-2.7, size=8) 
#FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=17, y=-1.95, size=8)  
FPrich_coeff
ggsave("model_avg_FPrich_raw_coeff.jpg",FPrich_coeff,height=8,width=11)


############################
# Combining multiple plots # 
############################
LM_coeff2 <- ggplot(avg_LM_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff2 <- LM_coeff2 + geom_bar(colour="black",stat="identity")
LM_coeff2 <- LM_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff2 <- LM_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff2 <- LM_coeff2 + xlab(NULL)
LM_coeff2 <- LM_coeff2 + ylab("Coefficient")
LM_coeff2 <- LM_coeff2 + theme_classic(base_size=18)
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=4, y=4.85, size=8) 
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=12, y=4.35, size=8) 
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=15, y=-4, size=8) 
LM_coeff2 <- LM_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
LM_coeff2 <- LM_coeff2 + geom_text(aes(1.25,5,label="a)"),fontface="bold")
LM_coeff2 <- LM_coeff2 + geom_hline(yintercept=0)
LM_coeff2

SP_coeff2 <- ggplot(avg_SP_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff2 <- SP_coeff2 + geom_bar(colour="black",stat="identity")
SP_coeff2 <- SP_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff2 <- SP_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff2 <- SP_coeff2 + xlab(NULL)
SP_coeff2 <- SP_coeff2 + ylab("Coefficient")
SP_coeff2 <- SP_coeff2 + theme_classic(base_size=18)
SP_coeff2 <- SP_coeff2 + annotate("text", label = "*", x=7, y=5.5, size=8) # signif. cond
SP_coeff2 <- SP_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
SP_coeff2 <- SP_coeff2 + geom_text(aes(1.25,5.5,label="b)"),fontface="bold")
SP_coeff2 <- SP_coeff2 + geom_hline(yintercept=0)
SP_coeff2

W_coeff2 <- ggplot(avg_W_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff2 <- W_coeff2 + geom_bar(colour="black",stat="identity")
W_coeff2 <- W_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff2 <- W_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff2 <- W_coeff2 + xlab("Predictor")
W_coeff2 <- W_coeff2 + ylab("Coefficient")
W_coeff2 <- W_coeff2 + theme_classic(base_size=18)
W_coeff2 <- W_coeff2 + annotate("text", label = "*", x=6, y=5.7, size=8) # signif. cond
W_coeff2 <- W_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
W_coeff2 <- W_coeff2 + geom_text(aes(1.25,6.45,label="c)"),fontface="bold")
W_coeff2 <- W_coeff2 + geom_hline(yintercept=0)
W_coeff2

FPpres_coeff2 <- ggplot(avg_FPpres_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff2 <- FPpres_coeff2 + geom_bar(colour="black",stat="identity")
FPpres_coeff2 <- FPpres_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff2 <- FPpres_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff2 <- FPpres_coeff2 + xlab("Predictor")
FPpres_coeff2 <- FPpres_coeff2 + ylab("Coefficient")
FPpres_coeff2 <- FPpres_coeff2 + theme_classic(base_size=18)
FPpres_coeff2 <- FPpres_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_coeff2 <- FPpres_coeff2 + geom_hline(yintercept=0)
FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=7, y=7, size=8) 
FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=4, y=5.95, size=8)  
FPpres_coeff2 <- FPpres_coeff2 + geom_text(aes(1.25,7.9,label="d)"),fontface="bold")
FPpres_coeff2

combined_coeff <- arrangeGrob(LM_coeff2,SP_coeff2,W_coeff2,FPpres_coeff2)

ggsave("model_avg_combined_raw_coeff.jpg",combined_coeff,height=8,width=16)



