###########################
# Boosted regression tree #
# FP richness             #
###########################
library(gbm)
library(dismo)

# Check out the column names to find out where your variables are 
colnames(data)

#################################
# A first guess @ the paramters #
#################################
# Identify the optimal number of trees (nt)
FPrich.tc2.lr001 <- gbm.fixed(data=data, 
                            gbm.x = c(10:12,30:35),
                            gbm.y = 29,
                            family = "poisson",
                            tree.complexity = 2,
                            learning.rate = 0.001,
                            bag.fraction = 0.5,
                            n.trees=2000
                            )
# Variable importance 
summary(FPrich.tc2.lr001)

FPrich.tc2.lr001$self.statistics

######################
# Simplify the model #
######################
# this only works if I used gbm.step() to create FPrich.tc2.lr001

# FPrich.tc2.lr001.simp <- gbm.simplify(FPrich.tc2.lr001)

# it doesn't say to drop any parameters 

############
# Plotting #
############
gbm.plot(FPrich.tc2.lr001,common.scale=F)

# plot the ﬁtted values in relation to each of the predictors
# Depending on the distribution of observations within the environmental space, fitted functions can
# give a misleading indication about the distribution of the fitted values in relation to each predictor.
# Values above each graph indicate the weighted mean of ﬁtted values in relation to each non-factor predictor
gbm.plot.fits(FPrich.tc2.lr001)

##########################################
#  Interrogate and plot the interactions #
##########################################
FPrich.tc2.lr001.interactions <- gbm.interactions(FPrich.tc2.lr001)
FPrich.tc2.lr001.interactions
FPrich.tc2.lr001.interactions$interactions
FPrich.tc2.lr001.interactions$rank.list

# pH and depth
gbm.perspec(FPrich.tc2.lr001, 3, 6)

# Alkalinity and depth
gbm.perspec(FPrich.tc2.lr001, 3,8)

# conductivity and total P
gbm.perspec(FPrich.tc2.lr001, 7, 5,theta=25)


