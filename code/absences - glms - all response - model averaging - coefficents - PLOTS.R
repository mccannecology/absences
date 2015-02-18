library(ggplot2)
library(MuMIn)
library(gridExtra)

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
summary(avg_LM_trans)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes1km",
                "lakes10km","waterfowl","distLM","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_LM_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_LM_trans_coeff$coefficient<- c(0.02984, 0.25991, -2.29132, 3.10344, 0.04890,
                                   2.51762, 1.16794, -0.86869, 0.05142, 0.00000,
                                   0.01659, 2.90924, 0.04566, 0.02208, -2.82474, 
                                   -0.01386)
avg_LM_trans_coeff$stand_err <- c(0.26050, 0.68957, 1.46905, 1.55563, 0.38878,
                                  1.72149, 1.85590, 1.61976, 0.36231, 0.00000,
                                  0.21027, 1.22403, 0.34572, 0.15815, 0.88529, 
                                  0.17918)
avg_LM_trans_coeff

# Re-order the variables 
avg_LM_trans_coeff$predictors <- factor(avg_LM_trans_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                "cond","alk","secchi","nonFP","lakes1km",
                                                                                "lakes10km","waterfowl","distLM","boatlaunch","latitude",
                                                                                "longitude") ) 

LM_coeff <- ggplot(avg_LM_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff <- LM_coeff + geom_bar(colour="black",stat="identity")
LM_coeff <- LM_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff <- LM_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff <- LM_coeff + xlab("Predictor")
LM_coeff <- LM_coeff + ylab("Coefficient")
LM_coeff <- LM_coeff + theme_classic(base_size=18)
LM_coeff <- LM_coeff + annotate("text", label = "*", x=4, y=4.85, size=8) 
LM_coeff <- LM_coeff + annotate("text", label = "*", x=12, y=4.35, size=8) 
LM_coeff <- LM_coeff + annotate("text", label = "*", x=15, y=-4, size=8) 
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
summary(avg_SP_trans)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes1km",
                "lakes10km","waterfowl","distSP","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_SP_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_SP_trans_coeff$coefficient<- c(0.421975, 0.000000, -0.452568, 0.140413, 0.000000,
                                   0.086691, 3.927619, -1.089119, 0.156880, -0.014993, 
                                   0.000000, 0.418043, 0.007841, 0.033060, -0.022901,
                                   0.000000)
avg_SP_trans_coeff$stand_err <- c(0.899402, 0.000000, 1.081935, 0.643284, 0.000000,
                                  0.529419, 1.413848, 1.511143, 0.639840, 0.183704,
                                  0.000000, 0.917485, 0.155822, 0.190585, 0.201808, 
                                  0.000000)
avg_SP_trans_coeff

# Re-order the variables 
avg_SP_trans_coeff$predictors <- factor(avg_SP_trans_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                "cond","alk","secchi","nonFP","lakes1km",
                                                                                "lakes10km","waterfowl","distSP","boatlaunch","latitude",
                                                                                "longitude") ) 

SP_coeff <- ggplot(avg_SP_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff <- SP_coeff + geom_bar(colour="black",stat="identity")
SP_coeff <- SP_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff <- SP_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff <- SP_coeff + xlab("Predictor")
SP_coeff <- SP_coeff + ylab("Coefficient")
SP_coeff <- SP_coeff + theme_classic(base_size=18)
SP_coeff <- SP_coeff + annotate("text", label = "*", x=7, y=5.5, size=8) # signif. cond
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
summary(avg_W_trans)


predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes1km",
                "lakes10km","waterfowl","distW","boatlaunch","latitude",
                "longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other",
                    "Regional - other") 
avg_W_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_W_trans_coeff$coefficient<- c(1.12957, -0.42067, -3.88436, 1.01685, 0.000000,
                                  4.00505, 0.06776, -0.29296, 0.07010, 0.000000,
                                  -1.59496, 0.06245, 0.18756, 1.17611, -0.92155,
                                  0.000000)
avg_W_trans_coeff$stand_err <- c(1.82731, 1.14088, 2.00560, 1.74718, 0.000000,
                                 1.48098, 0.54759, 1.10703, 0.49601, 0.000000,
                                 1.55374, 0.42429, 0.70771, 0.81761, 1.22076, 
                                 0.000000)
avg_W_trans_coeff


# Re-order the variables 
avg_W_trans_coeff$predictors <- factor(avg_W_trans_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                              "cond","alk","secchi","nonFP","lakes1km",
                                                                              "lakes10km","waterfowl","distW","boatlaunch","latitude",
                                                                              "longitude") ) 

W_coeff <- ggplot(avg_W_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff <- W_coeff + geom_bar(colour="black",stat="identity")
W_coeff <- W_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff <- W_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff <- W_coeff + xlab("Predictor")
W_coeff <- W_coeff + ylab("Coefficient")
W_coeff <- W_coeff + theme_classic(base_size=18)
W_coeff <- W_coeff + annotate("text", label = "*", x=6, y=5.7, size=8) # signif. cond
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
summary(avg_FPpres_trans)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes1km",
                "lakes10km","waterfowl","distLM","distSP","distW",
                "boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 
avg_FPpres_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPpres_trans_coeff$coefficient<- c(-0.004912, 0.142929, -0.548735, 4.222111, 0.000000,
                                       0.625932, 5.173975, -3.097537, 0.551640, -0.003840,
                                       0.136534, 0.354061, 0.000000, 0.069346, 0.000000,
                                       0.004002, -0.394519, -0.042255)
avg_FPpres_trans_coeff$stand_err <- c(0.108709, 0.523681, 1.094292, 1.608068, 0.000000,
                                      1.255263, 1.721056, 1.832164, 1.080720, 0.092108, 
                                      0.50001, 0.830579, 0.000000, 0.354922, 0.000000,
                                      0.071671, 0.721735, 0.258063)
avg_FPpres_trans_coeff

# Re-order the variables 
avg_FPpres_trans_coeff$predictors <- factor(avg_FPpres_trans_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                        "cond","alk","secchi","nonFP","lakes1km",
                                                                                        "lakes10km","waterfowl","distLM","distSP","distW",
                                                                                        "boatlaunch","latitude","longitude") ) 

FPpres_coeff <- ggplot(avg_FPpres_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff <- FPpres_coeff + geom_bar(colour="black",stat="identity")
FPpres_coeff <- FPpres_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff <- FPpres_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff <- FPpres_coeff + xlab("Predictor")
FPpres_coeff <- FPpres_coeff + ylab("Coefficient")
FPpres_coeff <- FPpres_coeff + theme_classic(base_size=18)
FPpres_coeff <- FPpres_coeff + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_coeff <- FPpres_coeff + geom_hline(yintercept=0)
FPpres_coeff <- FPpres_coeff + annotate("text", label = "*", x=7, y=7, size=8) 
FPpres_coeff <- FPpres_coeff + annotate("text", label = "*", x=4, y=5.95, size=8)  
FPpres_coeff
ggsave("model_avg_FPpres_coeff.jpg",FPpres_coeff,height=8,width=11)

# re-save without a legend 
temp <- FPpres_coeff
temp <- temp + theme(legend.position="none")
temp <- temp + ggtitle("")
temp 
ggsave("model_avg_FPpres_coeff-no_legend.jpg",temp,height=8,width=11)


#######################
# Model Average       #
# Coefficients        #
# Plotting            #
# FPrichness          #
#######################
summary(avg_FPrich_trans)

predictors <- c("size","shoreline","depth","totalP","pH",
                "cond","alk","secchi","nonFP","lakes1km",
                "lakes10km","waterfowl","distLM","distSP","distW",
                "boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic",
                    "Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic","Regional - dispersal",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - dispersal","Regional - other","Regional - other") 
avg_FPrich_trans_coeff <- as.data.frame(cbind(predictors,predictor_type))
avg_FPrich_trans_coeff$coefficient<- c(0.34707, 0.000000, -1.67005, 1.11753, 0.000000,
                                       2.10504, 1.07678, -0.38115, 0.19012, 0.000000,
                                       0.000000, 0.87672, 1.06836, 0.05404, 0.01618, 
                                       0.20692, -1.29290, 0.000000)
avg_FPrich_trans_coeff$stand_err <- c(0.57344, 0.000000, 0.78855, 0.82063, 0.000000,
                                      0.75222, 0.94250, 0.77822, 0.48116, 0.000000,
                                      0.000000, 0.61056, 0.72960, 0.23406, 0.13416,
                                      0.29086, 0.45100, 0.000000)
avg_FPrich_trans_coeff

# Re-order the variables 
avg_FPrich_trans_coeff$predictors <- factor(avg_FPrich_trans_coeff$predictors, levels=c("size","shoreline","depth","totalP","pH",
                                                                                        "cond","alk","secchi","nonFP","lakes1km",
                                                                                        "lakes10km","waterfowl","distLM","distSP","distW",
                                                                                        "boatlaunch","latitude","longitude") ) 

FPrich_coeff <- ggplot(avg_FPrich_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPrich_coeff <- FPrich_coeff + geom_bar(colour="black",stat="identity")
FPrich_coeff <- FPrich_coeff + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPrich_coeff <- FPrich_coeff + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPrich_coeff <- FPrich_coeff + xlab("Predictor")
FPrich_coeff <- FPrich_coeff + ylab("Coefficient")
FPrich_coeff <- FPrich_coeff + theme_classic(base_size=18)
FPrich_coeff <- FPrich_coeff + theme(axis.text.x = element_text(angle=45,hjust=1))
#FPrich_coeff <- FPrich_coeff + ggtitle("Floating plant richness")
FPrich_coeff <- FPrich_coeff + geom_hline(yintercept=0)
FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=6, y=2.95, size=8) 
FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=3, y=-2.7, size=8) 
FPrich_coeff <- FPrich_coeff + annotate("text", label = "*", x=17, y=-1.95, size=8)  
FPrich_coeff
ggsave("model_avg_FPrich_coeff.jpg",FPrich_coeff,height=8,width=11)


############################
# Combining multiple plots # 
############################
LM_coeff2 <- ggplot(avg_LM_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff2 <- LM_coeff2 + geom_bar(colour="black",stat="identity")
LM_coeff2 <- LM_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff2 <- LM_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff2 <- LM_coeff2 + xlab(NULL)
LM_coeff2 <- LM_coeff2 + ylab("Coefficient")
LM_coeff2 <- LM_coeff2 + theme_classic(base_size=18)
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=4, y=4.85, size=8) 
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=12, y=4.35, size=8) 
LM_coeff2 <- LM_coeff2 + annotate("text", label = "*", x=15, y=-4, size=8) 
LM_coeff2 <- LM_coeff2 + theme(axis.text.x = element_text(angle=45, hjust=1))
LM_coeff2 <- LM_coeff2 + geom_text(aes(1.25,5,label="a)"),fontface="bold")
LM_coeff2 <- LM_coeff2 + geom_hline(yintercept=0)
LM_coeff2

SP_coeff2 <- ggplot(avg_SP_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff2 <- SP_coeff2 + geom_bar(colour="black",stat="identity")
SP_coeff2 <- SP_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff2 <- SP_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff2 <- SP_coeff2 + xlab(NULL)
SP_coeff2 <- SP_coeff2 + ylab("Coefficient")
SP_coeff2 <- SP_coeff2 + theme_classic(base_size=18)
SP_coeff2 <- SP_coeff2 + annotate("text", label = "*", x=7, y=5.5, size=8) # signif. cond
SP_coeff2 <- SP_coeff2 + theme(axis.text.x = element_text(angle=45, hjust=1))
SP_coeff2 <- SP_coeff2 + geom_text(aes(1.25,5.5,label="b)"),fontface="bold")
SP_coeff2 <- SP_coeff2 + geom_hline(yintercept=0)
SP_coeff2

W_coeff2 <- ggplot(avg_W_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff2 <- W_coeff2 + geom_bar(colour="black",stat="identity")
W_coeff2 <- W_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff2 <- W_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff2 <- W_coeff2 + xlab("Predictor")
W_coeff2 <- W_coeff2 + ylab("Coefficient")
W_coeff2 <- W_coeff2 + theme_classic(base_size=18)
W_coeff2 <- W_coeff2 + annotate("text", label = "*", x=6, y=5.7, size=8) # signif. cond
W_coeff2 <- W_coeff2 + theme(axis.text.x = element_text(angle=45, hjust=1))
W_coeff2 <- W_coeff2 + geom_text(aes(1.25,6.45,label="c)"),fontface="bold")
W_coeff2 <- W_coeff2 + geom_hline(yintercept=0)
W_coeff2

FPpres_coeff2 <- ggplot(avg_FPpres_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff2 <- FPpres_coeff2 + geom_bar(colour="black",stat="identity")
FPpres_coeff2 <- FPpres_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff2 <- FPpres_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff2 <- FPpres_coeff2 + xlab("Predictor")
FPpres_coeff2 <- FPpres_coeff2 + ylab("Coefficient")
FPpres_coeff2 <- FPpres_coeff2 + theme_classic(base_size=18)
FPpres_coeff2 <- FPpres_coeff2 + theme(axis.text.x = element_text(angle=45, hjust=1))
FPpres_coeff2 <- FPpres_coeff2 + geom_hline(yintercept=0)
FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=7, y=7, size=8) 
FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=4, y=5.95, size=8)  
FPpres_coeff2 <- FPpres_coeff2 + geom_text(aes(1.25,7.9,label="d)"),fontface="bold")
FPpres_coeff2

combined_coeff <- arrangeGrob(LM_coeff2,SP_coeff2,W_coeff2,FPpres_coeff2)

ggsave("model_avg_combined_coeff.jpg",combined_coeff,height=8,width=16)



#######################################
# Combined plot for the three species # 
#######################################
LM_coeff2 <- ggplot(avg_LM_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
LM_coeff2 <- LM_coeff2 + geom_bar(colour="black",stat="identity")
LM_coeff2 <- LM_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
LM_coeff2 <- LM_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_coeff2 <- LM_coeff2 + xlab("Predictor")
LM_coeff2 <- LM_coeff2 + ylab("Coefficient")
LM_coeff2 <- LM_coeff2 + theme_classic(base_size=18)
LM_coeff2 <- LM_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
LM_coeff2 <- LM_coeff2 + geom_hline(yintercept=0)
LM_coeff2 <- LM_coeff2 + geom_text(aes(1.25,5,label="a)"),fontface="bold")
LM_coeff2

SP_coeff2 <- ggplot(avg_SP_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
SP_coeff2 <- SP_coeff2 + geom_bar(colour="black",stat="identity")
SP_coeff2 <- SP_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
SP_coeff2 <- SP_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_coeff2 <- SP_coeff2 + xlab("Predictor")
SP_coeff2 <- SP_coeff2 + ylab("Coefficient")
SP_coeff2 <- SP_coeff2 + theme_classic(base_size=18)
SP_coeff2 <- SP_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
SP_coeff2 <- SP_coeff2 + geom_hline(yintercept=0)
SP_coeff2 <- SP_coeff2 + geom_text(aes(1.25,3.9,label="b)"),fontface="bold")
SP_coeff2

W_coeff2 <- ggplot(avg_W_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
W_coeff2 <- W_coeff2 + geom_bar(colour="black",stat="identity")
W_coeff2 <- W_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
W_coeff2 <- W_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_coeff2 <- W_coeff2 + xlab("Predictor")
W_coeff2 <- W_coeff2 + ylab("Coefficient")
W_coeff2 <- W_coeff2 + theme_classic(base_size=18)
W_coeff2 <- W_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
W_coeff2 <- W_coeff2 + geom_hline(yintercept=0)
W_coeff2 <- W_coeff2 + geom_text(aes(1.25,4.75,label="c)"),fontface="bold")
W_coeff2

FPpres_coeff2 <- ggplot(avg_FPpres_trans_coeff, aes(x=predictors,y=coefficient,fill=predictor_type))
FPpres_coeff2 <- FPpres_coeff2 + geom_bar(colour="black",stat="identity")
FPpres_coeff2 <- FPpres_coeff2 + geom_errorbar(aes(ymin=coefficient-stand_err,ymax=coefficient+stand_err), width=0.2, position=position_dodge(0.9))
FPpres_coeff2 <- FPpres_coeff2 + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_coeff2 <- FPpres_coeff2 + xlab("Predictor")
FPpres_coeff2 <- FPpres_coeff2 + ylab("Coefficient")
FPpres_coeff2 <- FPpres_coeff2 + theme_classic(base_size=18)
FPpres_coeff2 <- FPpres_coeff2 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_coeff2 <- FPpres_coeff2 + geom_hline(yintercept=0)
#FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=7, y=7.9, size=8) 
#FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=4, y=5.8, size=8)  
#FPpres_coeff2 <- FPpres_coeff2 + annotate("text", label = "*", x=15, y=4, size=8) 
FPpres_coeff2

coeff_plot_combined <- arrangeGrob(LM_coeff2,SP_coeff2,W_coeff2)