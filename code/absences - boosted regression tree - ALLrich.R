###########################
# Boosted regression tree #
# All plant richness      #
###########################
library(gbm)
library(dismo)

# Check out the column names to find out where your variables are 
colnames(data)

# Excluded waterbodies_5km and nonFP_species_richness
# Included depth_max_m and ALK_avg

#################################
# A first guess @ the paramters #
#################################
# Identify the optimal number of trees (nt)
all_rich.tc2.lr001 <- gbm.step(data=data, 
                              gbm.x = c(5,6,10:12,31:35,153:162),
                              gbm.y = 28,
                              family = "poisson",
                              tree.complexity = 2,
                              learning.rate = 0.001,
                              bag.fraction = 0.5,
                              plot.main=FALSE
                              )
# Variable importance 
summary(all_rich.tc2.lr001,plot=FALSE)
summary(all_rich.tc2.lr001)


all_rich.tc2.lr001$self.statistics

######################
# Simplify the model #
######################
# this only works if I used gbm.step() to create all_rich.tc2.lr001

# all_rich.tc2.lr001.simp <- gbm.simplify(all_rich.tc2.lr001)

# it doesn't say to drop any parameters 

############
# Plotting #
############
gbm.plot(all_rich.tc2.lr001,common.scale=F,plot.layout=c(4, 4))

# just the first 6 most important 
# save the file 
jpeg("BRT_all_plant_richness.jpg",height=8,width=11,units="in",res=300)
gbm.plot(all_rich.tc2.lr001,common.scale=F,n.plots=6,plot.layout=c(2, 3),write.title=FALSE)
title(main="Boosted Regression Tree: Y=All plant richness")
dev.off()

# plot the ﬁtted values in relation to each of the predictors
# Depending on the distribution of observations within the environmental space, fitted functions can
# give a misleading indication about the distribution of the fitted values in relation to each predictor.
# Values above each graph indicate the weighted mean of ﬁtted values in relation to each non-factor predictor
gbm.plot.fits(all_rich.tc2.lr001)

##########################################
#  Interrogate and plot the interactions #
##########################################
all_rich.tc2.lr001.interactions <- gbm.interactions(all_rich.tc2.lr001)
all_rich.tc2.lr001.interactions
all_rich.tc2.lr001.interactions$interactions
all_rich.tc2.lr001.interactions$rank.list

# pH and depth
gbm.perspec(all_rich.tc2.lr001, 3, 6)

# conductivity and total P
gbm.perspec(all_rich.tc2.lr001, 7, 5,theta=25)

# waterbodies_1km and COND_avg
gbm.perspec(all_rich.tc2.lr001, 10,7)
            
# Alkalinity and depth
gbm.perspec(all_rich.tc2.lr001, 3,8)

###################################
# re-try without any interactions #
###################################
# use the # of trees from the full model
all_rich.tc2.lr001.nointeraction <- gbm.fixed(data=data, 
                              gbm.x = c(10:12,30:35,153,155:157),
                              gbm.y = 28,
                              family = "poisson",
                              tree.complexity = 1,
                              learning.rate = 0.001,
                              bag.fraction = 0.5,
                              n.trees=2000
)

# Variable importance 
summary(all_rich.tc2.lr001.nointeraction)

all_rich.tc2.lr001.nointeraction$self.statistics


#######################
# re-try              #
# excluded depth, ALK #
#######################
# use the # of trees from the full model
all_rich.tc2.lr001.excludedvars <- gbm.fixed(data=data, 
                                            c(10:11,30:33,35,153,155:157),
                                            gbm.y = 28,
                                            family = "poisson",
                                            tree.complexity = 1,
                                            learning.rate = 0.001,
                                            bag.fraction = 0.5,
                                            n.trees=2000
)

# Variable importance 
summary(all_rich.tc2.lr001.excludedvars)

all_rich.tc2.lr001.excludedvars$self.statistics

