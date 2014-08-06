###################################
# Boosted regression tree         #
# Spirodela polyrhzia presence    #
##################################
library(gbm)
library(dismo)

# Check out the column names to find out where your variables are 
colnames(data)

# Excluded waterbodies_5km 
# Included depth_max_m and ALK_avg

#################################
# A first guess @ the paramters #
#################################
# Identify the optimal number of trees (nt)
SP.tc2.lr001 <- gbm.fixed(data=data, 
                            gbm.x = c(10:12,30:35,153,155:157),
                            gbm.y = 24,
                            family = "bernoulli",
                            tree.complexity = 2,
                            learning.rate = 0.001,
                            bag.fraction = 0.5,
                            n.trees=2000
                            )
# Variable importance 
summary(SP.tc2.lr001)

SP.tc2.lr001$self.statistics

######################
# Simplify the model #
######################
# this only works if I used gbm.step() to create SP.tc2.lr001

# SP.tc2.lr001.simp <- gbm.simplify(SP.tc2.lr001)

# it doesn't say to drop any parameters 

############
# Plotting #
############
gbm.plot(SP.tc2.lr001,common.scale=F)


# just the first 6 most important 
# save the file 
jpeg("BRT_SP.jpg",height=8,width=11,units="in",res=300)
gbm.plot(SP.tc2.lr001,common.scale=F,n.plots=6,plot.layout=c(2, 3),write.title=FALSE)
title(main="Boosted Regression Tree: Y=S. polyrhiza(presence)")
dev.off()


# plot the ﬁtted values in relation to each of the predictors
# Depending on the distribution of observations within the environmental space, fitted functions can
# give a misleading indication about the distribution of the fitted values in relation to each predictor.
# Values above each graph indicate the weighted mean of ﬁtted values in relation to each non-factor predictor
gbm.plot.fits(SP.tc2.lr001)

##########################################
#  Interrogate and plot the interactions #
##########################################
SP.tc2.lr001.interactions <- gbm.interactions(SP.tc2.lr001)
SP.tc2.lr001.interactions
SP.tc2.lr001.interactions$interactions
SP.tc2.lr001.interactions$rank.list

# pH and depth
gbm.perspec(SP.tc2.lr001, 3, 6)

# conductivity and total P
gbm.perspec(SP.tc2.lr001, 7, 5,theta=25)

# Alkalinity and conductivity
gbm.perspec(SP.tc2.lr001, 7, 8)

# conductivity and non-FP richness
gbm.perspec(SP.tc2.lr001, 7, 4)

