###########################
# Boosted regression tree #
# Lemna minor presence    #
###########################
library(gbm)
library(dismo)

# Check out the column names to find out where your variables are 
colnames(data)

# inlclude all possible predictor variables

#################################
# A first guess @ the paramters #
#################################
# Identify the optimal number of trees (nt)
LM.tc2.lr001 <- gbm.step(data=data, 
                          gbm.x = c(5,6,10:12,30:35,153:162),
                          gbm.y = 20,
                          family = "bernoulli",
                          tree.complexity = 2,
                          learning.rate = 0.001,
                          bag.fraction = 0.5)
                         
# Variable importance 
summary(LM.tc2.lr001)

LM.tc2.lr001$self.statistics


# A fixed number of trees 
LM.tc2.lr001 <- gbm.fixed(data=data, 
                          gbm.x = c(5,6,10:12,30:35,153:162),
                          gbm.y = 20,
                            family = "bernoulli",
                            tree.complexity = 2,
                            learning.rate = 0.001,
                            bag.fraction = 0.5,
                            n.trees=10000
                            )
# Variable importance 
summary(LM.tc2.lr001,plot=F)

LM.tc2.lr001$self.statistics

# simplify - variable removal proceeds until average change exceeds the original se
LM.tc2.lr001.simp <- gbm.simplify(LM.tc2.lr001)

# drop 4 variables
# 1 - boatlaunch 
# 2 - major_watershed 
# 3 - nearest_SP 
# 4 - nearest_W 

######################
# Simplify the model #
######################
# this only works if I used gbm.step() to create LM.tc2.lr001

# LM.tc2.lr001.simp <- gbm.simplify(LM.tc2.lr001)

# it doesn't say to drop any parameters 

############
# Plotting #
############
# just the first 6 most important 
# save the file 
jpeg("BRT_LM.jpg",height=8,width=11,units="in",res=300)
gbm.plot(LM.tc2.lr001,common.scale=F,n.plots=6,plot.layout=c(2, 3),write.title=FALSE)
title(main="Boosted Regression Tree: Y=L. minor(presence)")
dev.off()

# plot the ﬁtted values in relation to each of the predictors
# Depending on the distribution of observations within the environmental space, fitted functions can
# give a misleading indication about the distribution of the fitted values in relation to each predictor.
# Values above each graph indicate the weighted mean of ﬁtted values in relation to each non-factor predictor
gbm.plot.fits(LM.tc2.lr001)

##########################################
#  Interrogate and plot the interactions #
##########################################
LM.tc2.lr001.interactions <- gbm.interactions(LM.tc2.lr001)
LM.tc2.lr001.interactions
LM.tc2.lr001.interactions$interactions
LM.tc2.lr001.interactions$rank.list

# waterbodies_10km and TOTP_avg
gbm.perspec(LM.tc2.lr001, 11, 5)

# waterbodies_1km and COND_avg
gbm.perspec(LM.tc2.lr001, 10, 7)

# total P and depth
gbm.perspec(LM.tc2.lr001, 3, 5)

# secchi and total P
gbm.perspec(LM.tc2.lr001, 9,5)

# conductivity and alkalinity 
gbm.perspec(LM.tc2.lr001, 7, 8,theta=15)

# alkalinity and depth 
gbm.perspec(LM.tc2.lr001, 3, 8)

