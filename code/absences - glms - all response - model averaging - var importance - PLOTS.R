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
# Variable importance #
# Plotting            #
# LEMNA               # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_LM_trans_var_imp <- as.data.frame(cbind(predictors,predictor_type))
avg_LM_trans_var_imp$importance <- c(0, 0.0821357, 0.9169658, 0.1071323, 1.0000000, 1.0000000, 0,
                                     1.0000000, 0.2051784, 0.8169590, 0.8871801, 0, 1.0000000, 0.1063860)
avg_LM_trans_var_imp

# Re-order the variables 
avg_LM_trans_var_imp$predictors <- factor(avg_LM_trans_var_imp$predictors, levels=c("size","shoreline",
                                                                                    "totalP","pH","cond","secchi",
                                                                                    "nonFP","lakes_1km",
                                                                                    "lakes_10km","waterfowl","dist_LM",
                                                                                    "boatlaunch","latitude","longitude") ) 

LM_var_imp <- ggplot(avg_LM_trans_var_imp, aes(x=predictors,y=importance,fill=predictor_type))
LM_var_imp <- LM_var_imp + geom_bar(colour="black",stat="identity")
LM_var_imp <- LM_var_imp + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
LM_var_imp <- LM_var_imp + xlab("Predictor")
LM_var_imp <- LM_var_imp + ylab("Relative variable importance ")
LM_var_imp <- LM_var_imp + theme_classic(base_size=18)
LM_var_imp <- LM_var_imp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
LM_var_imp <- LM_var_imp + ggtitle("Lemna minor")
LM_var_imp
ggsave("model_avg_LM_var_imp.jpg",LM_var_imp,height=8,width=11)

#######################
# Model Average       #
# Variable importance #
# Plotting            #
# SPIRODELA           # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_S","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_SP_trans_var_imp <- as.data.frame(cbind(predictors,predictor_type))
avg_SP_trans_var_imp$importance <- c(0,0.04442046,0.18324640,0.04718724,1.00000000,0.35527101,0.05190428,
                                     0.10310221,0.04607764,0.05413708,0,0.09097746,0,0.04499529)
avg_SP_trans_var_imp

# Re-order the variables 
avg_SP_trans_var_imp$predictors <- factor(avg_SP_trans_var_imp$predictors, levels=c("size","shoreline",
                                                                                    "totalP","pH","cond","secchi",
                                                                                    "nonFP","lakes_1km",
                                                                                    "lakes_10km","waterfowl","dist_S",
                                                                                    "boatlaunch","latitude","longitude") ) 

SP_var_imp <- ggplot(avg_SP_trans_var_imp, aes(x=predictors,y=importance,fill=predictor_type))
SP_var_imp <- SP_var_imp + geom_bar(colour="black",stat="identity")
SP_var_imp <- SP_var_imp + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
SP_var_imp <- SP_var_imp + xlab("Predictor")
SP_var_imp <- SP_var_imp + ylab("Relative variable importance ")
SP_var_imp <- SP_var_imp + theme_classic(base_size=18)
SP_var_imp <- SP_var_imp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
SP_var_imp <- SP_var_imp + ggtitle("Spirodela polyrhiza")
SP_var_imp
ggsave("model_avg_SP_var_imp.jpg",SP_var_imp,height=8,width=11)

#######################
# Model Average       #
# Variable importance #
# Plotting            #
# WOLFFIA             # 
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_W","boatlaunch","latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - other","Regional - other") 
avg_W_trans_var_imp <- as.data.frame(cbind(predictors,predictor_type))
avg_W_trans_var_imp$importance <- c(0,0.19862020,0.33535364,0,1.00000000,0.93051719,0,
                                    0.18705228,0.34108152,0.07078293,0,0.72567276,0.06813716,0)
avg_W_trans_var_imp


# Re-order the variables 
avg_W_trans_var_imp$predictors <- factor(avg_W_trans_var_imp$predictors, levels=c("size","shoreline",
                                                                                  "totalP","pH","cond","secchi",
                                                                                  "nonFP","lakes_1km",
                                                                                  "lakes_10km","waterfowl","dist_W",
                                                                                  "boatlaunch","latitude","longitude") ) 

W_var_imp <- ggplot(avg_W_trans_var_imp, aes(x=predictors,y=importance,fill=predictor_type))
W_var_imp <- W_var_imp + geom_bar(colour="black",stat="identity")
W_var_imp <- W_var_imp + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
W_var_imp <- W_var_imp + xlab("Predictor")
W_var_imp <- W_var_imp + ylab("Relative variable importance ")
W_var_imp <- W_var_imp + theme_classic(base_size=18)
W_var_imp <- W_var_imp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
W_var_imp <- W_var_imp + ggtitle("Wolffia sp.")
W_var_imp
ggsave("model_avg_W_var_imp.jpg",W_var_imp,height=8,width=11)

#######################
# Model Average       #
# Variable importance #
# Plotting            #
# FPpresence          #
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","dist_S","dist_W","boatlaunch",
                "latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - other","Regional - other")
avg_FPpres_trans_var_imp <- as.data.frame(cbind(predictors,predictor_type))
avg_FPpres_trans_var_imp$importance <- c(0, 0.02784981, 1.00000000, 0.35947343, 1.00000000, 1.00000000, 0.08974981,
                                         0.17603765, 0.13878970, 0.05543712, 1.00000000, 0.80645464, 0.08849221, 0,
                                         0.97088174,0.36866438)
avg_FPpres_trans_var_imp

# Re-order the variables 
avg_FPpres_trans_var_imp$predictors <- factor(avg_FPpres_trans_var_imp$predictors, levels=c("size","shoreline",
                                                                                            "totalP","pH","cond","secchi",
                                                                                            "nonFP","lakes_1km",
                                                                                            "lakes_10km","waterfowl",
                                                                                            "dist_LM","dist_S","dist_W",
                                                                                            "boatlaunch","latitude","longitude") ) 

FPpres_var_imp <- ggplot(avg_FPpres_trans_var_imp, aes(x=predictors,y=importance,fill=predictor_type))
FPpres_var_imp <- FPpres_var_imp + geom_bar(colour="black",stat="identity")
FPpres_var_imp <- FPpres_var_imp + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPpres_var_imp <- FPpres_var_imp + xlab("Predictor")
FPpres_var_imp <- FPpres_var_imp + ylab("Relative variable importance ")
FPpres_var_imp <- FPpres_var_imp + theme_classic(base_size=18)
FPpres_var_imp <- FPpres_var_imp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPpres_var_imp <- FPpres_var_imp + ggtitle("Floating plant presence")
FPpres_var_imp
ggsave("model_avg_FPpres_var_imp.jpg",FPpres_var_imp,height=8,width=11)

#######################
# Model Average       #
# Variable importance #
# Plotting            #
# FPrichness          #
#######################
predictors <- c("size","shoreline","totalP","pH","cond","secchi","nonFP",
                "lakes_1km","lakes_10km","waterfowl","dist_LM","dist_S","dist_W","boatlaunch",
                "latitude","longitude")
predictor_type <- c("Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - abiotic","Local - biotic",
                    "Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal","Regional - dispersal",
                    "Regional - other","Regional - other") 
avg_FPrich_trans_var_imp <- as.data.frame(cbind(predictors,predictor_type))
avg_FPrich_trans_var_imp$importance <- c(0, 0, 0.94229786, 0.12404552, 1.00000000, 1.00000000, 0.05035712,
                                         0.90281939, 0, 0.81942617, 1.00000000, 0.36594245, 0.04713825, 0.11624054,
                                         1.00000000,0.04685695)
avg_FPrich_trans_var_imp

# Re-order the variables 
avg_FPrich_trans_var_imp$predictors <- factor(avg_FPrich_trans_var_imp$predictors, levels=c("size","shoreline",
                                                                                            "totalP","pH","cond","secchi",
                                                                                            "nonFP","lakes_1km",
                                                                                            "lakes_10km","waterfowl",
                                                                                            "dist_LM","dist_S","dist_W",
                                                                                            "boatlaunch","latitude","longitude") ) 

FPrich_var_imp <- ggplot(avg_FPrich_trans_var_imp, aes(x=predictors,y=importance,fill=predictor_type))
FPrich_var_imp <- FPrich_var_imp + geom_bar(colour="black",stat="identity")
FPrich_var_imp <- FPrich_var_imp + scale_fill_manual(values=c("grey95","grey75","grey25","black"),name="Predictor type")
FPrich_var_imp <- FPrich_var_imp + xlab("Predictor")
FPrich_var_imp <- FPrich_var_imp + ylab("Relative variable importance ")
FPrich_var_imp <- FPrich_var_imp + theme_classic(base_size=18)
FPrich_var_imp <- FPrich_var_imp + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
FPrich_var_imp <- FPrich_var_imp + ggtitle("Floating plant richness")
FPrich_var_imp
ggsave("model_avg_FPrich_var_imp.jpg",FPrich_var_imp,height=8,width=11)
