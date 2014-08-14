###########################
# Boosted regression tree #
# Wolffia sp. presence    #
###########################
library(gbm)
library(dismo)

# Check out the column names to find out where your variables are 
colnames(data)

# inlclude all possible predictor variables

# add Wolffia (combined) to data 
# data$wolffia_all <- dataFP$wolffia
# already done 

##################################
# A first guess @ the parameters #
##################################
# Identify the optimal number of trees (nt)
W.tc2.lr001 <- gbm.step(data=data, 
                            gbm.x = c(5,6,10:12,30:35,153:162),
                            gbm.y = 164,
                            family = "bernoulli",
                            tree.complexity = 2,
                            learning.rate = 0.001,
                            bag.fraction = 0.5,
                            plot.main=FALSE
                            )
# Variable importance 
summary(W.tc2.lr001,plot=FALSE)

W.tc2.lr001$self.statistics

######################
# Simplify the model #
######################
# this only works if I used gbm.step() to create W.tc2.lr001

# W.tc2.lr001.simp <- gbm.simplify(W.tc2.lr001)

# it doesn't say to drop any parameters 

############
# Plotting #
############
gbm.plot(W.tc2.lr001,common.scale=F)

# just the first 6 most important 
# save the file 
jpeg("BRT_W.jpg",height=8,width=11,units="in",res=300)
gbm.plot(W.tc2.lr001,common.scale=F,n.plots=6,plot.layout=c(2, 3),write.title=FALSE)
title(main="Boosted Regression Tree: Y=Wolffia sp.(presence)")
dev.off()

# plot the ﬁtted values in relation to each of the predictors
# Depending on the distribution of observations within the environmental space, fitted functions can
# give a misleading indication about the distribution of the fitted values in relation to each predictor.
# Values above each graph indicate the weighted mean of ﬁtted values in relation to each non-factor predictor
gbm.plot.fits(W.tc2.lr001)

##########################################
#  Interrogate and plot the interactions #
##########################################
W.tc2.lr001.interactions <- gbm.interactions(W.tc2.lr001)
W.tc2.lr001.interactions
W.tc2.lr001.interactions$interactions
W.tc2.lr001.interactions$rank.list

# conductivity and total P
gbm.perspec(W.tc2.lr001, 7, 5,theta=25)

# pH and conductivity
gbm.perspec(W.tc2.lr001, 7, 6,theta=25)

# Alkalinity and secchi
gbm.perspec(W.tc2.lr001, 9,8)

# depth and surfacea area 
gbm.perspec(W.tc2.lr001, 3,1)




