########################
# GLMs                 #
# Y = species presence #
# 8/25/2014            #
########################

###############################################
# Build full and null models for everyspecies #
###############################################

# get your predictors cleaned-up 
colnames(dataENV_trans)
temp_data <- dataENV_trans # assign predictors to a temporary data frame 
temp_data$nearest_LM <- NULL  # remove nearest LM
temp_data$nearest_SP <- NULL  # remove nearest SP
temp_data$nearest_W <- NULL   # remove nearest W
colnames(temp_data) # check out the predictors you have now 

glm_results_allspecies <- list() 

# loop through every species (columns 36 to 152 of data)
for (i in unique(colnames(data)[c(18:27,36:152)])){
  
  ########################################
  # Start w/ fresh data frame every time #
  ########################################
  temp_data <- dataENV_trans # assign predictors to a temporary data frame 
  temp_data$nearest_LM <- NULL  # remove nearest LM
  temp_data$nearest_SP <- NULL  # remove nearest SP
  temp_data$nearest_W <- NULL   # remove nearest W
    
  ####################
  # Make the formula #
  ####################
  formula_glm <- as.formula(paste(i," ~ ", paste(colnames(temp_data), collapse= "+")))
  #print(formula_glm)
  
  ###################################
  # Assign the species to temp_data #
  ###################################
  temp_data <- cbind(temp_data, data[i])
  
  ####################
  # GLM - full model #
  ####################
  glm_full <- glm(formula_glm, family=binomial, data=temp_data, na.action = "na.fail")
  #summary(glm_full)
  
  ####################
  # Prop. dev. expl. #
  ####################
  null_dev <- as.numeric(summary(glm_full)[8]) # Null Deviance      
  resid_dev <- deviance(glm_full) # Residual Deviance
  # do the calculations   
  glm_full_devExpl <- (null_dev - resid_dev) / null_dev
  # print(glm_full_devExpl)
  
  ##################
  # Return results #
  ##################
  print(c(i,
          glm_full_devExpl,
          rownames(subset(summary(glm_full)$coefficients, summary(glm_full)$coefficients[,4] <= 0.05)) # just return names of significant predictors
          ))
  
  #glm_results_allspecies[[i]] <- summary(glm_full)[12]
  
}

###################################
# Prop. dev. expl. vs. spp. freq. #
###################################
# combine the two data frames (prop. expl. dev. & freq. of occurence) 
dataSPECIES_freq <- read.csv("dataSPECIES_freq.csv")
data_allSP_dev_expl <- read.csv("glm_all_species_dev_expl.csv")

colnames(dataSPECIES_freq)
colnames(data_allSP_dev_expl)

nrow(dataSPECIES_freq)
nrow(data_allSP_dev_expl)

data_temp <- merge(x=dataSPECIES_freq,y=data_allSP_dev_expl,by.x="species",by.y="taxa")

colnames(data_temp)[4] <- "Group"

jpeg(filename="glm_all_species_dev_expl_vs_freq.jpg",height=6,width=6,units="in",res=300)
plot(data_temp$frequency,data_temp$prop_dev_expl,xlab="Frequency",ylab="Proportion Deviance Explained")
dev.off()

# do the exact same plot but with ggplot2
# it will be easier to label points 
# colour/fill for "Group" is not plotting correctly - is this b/c there are empty values? 
dev_expl_vs_freq_plot <- ggplot(data_temp,aes(x=frequency,y=prop_dev_expl,colour=Group))
dev_expl_vs_freq_plot <- dev_expl_vs_freq_plot + geom_point(size=3,colour=Group)
dev_expl_vs_freq_plot <- dev_expl_vs_freq_plot + theme_classic(base_size=18)
dev_expl_vs_freq_plot <- dev_expl_vs_freq_plot + xlab("Frequency")
dev_expl_vs_freq_plot <- dev_expl_vs_freq_plot + ylab("Prop. Dev. Expl. by full glm")
dev_expl_vs_freq_plot




######################################
# Plot prop. dev. expl. for all spp. #
######################################
data_allSP_dev_expl <- read.csv("glm_all_species_dev_expl.csv") # Import 
# data_allSP_dev_expl[,3:8] <- NULL # Clean up
colnames(data_allSP_dev_expl) # view column names 

dev_expl_plot <- ggplot(data_allSP_dev_expl,aes(x=reorder(taxa,-prop_dev_expl),y=prop_dev_expl,fill=group))
dev_expl_plot <- dev_expl_plot + geom_bar(stat="identity")
dev_expl_plot <- dev_expl_plot + theme_classic(base_size=10)
dev_expl_plot <- dev_expl_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev_expl_plot <- dev_expl_plot + xlab("Species")
dev_expl_plot <- dev_expl_plot + ylab("Prop. Dev. Expl. by full glm")
dev_expl_plot <- dev_expl_plot + geom_text(aes(y=prop_dev_expl+0.01,label=label))
#dev_expl_plot <- dev_expl_plot + ggtitle("Bornette et al. 1998 - 23 cut-off channels Rhone R., France")
dev_expl_plot

ggsave("glm_all_species_dev_expl_plot.jpg",dev_expl_plot,height=8,width=11)

#####################################################
# Plot prop. dev. expl. vs. commonness for all spp. #
#####################################################