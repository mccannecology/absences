#####################################
# Permutation test                  #    
# Aquatic plant functional groups   # 
# CT Agricultural Exp. Station data #
# MJM 9/17/2014                     #
#####################################
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - permutations.RData")


# Question: 
# Is the freq. of occurence/absence of a functional group different than expected based on species richness of the functional group in the region? 
# More specificially: 
# Is the freq. of occurence of the floating plant functional group just a function of their low regional richness?

library(ggplot2)

# import and set-up your data frame 
data_permutation <- read.csv("dataSPECIES_freq3_with_func_group_labels.csv")
data_permutation$Label <- NULL # remove variables that you don't need 
data_permutation$FP. <- NULL


# enter your number of lakes & the number of iterations to do 
numb_lakes <- 176
numb_iterations <- 100

# set-up the data frame to hold your results 
permutation_results <- matrix(data=0, nrow=numb_iterations, ncol=4)
permutation_results <- as.data.frame(permutation_results)
colnames(permutation_results) <- c("emergent","floating","lily","submerged")

# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- as.character(data_permutation$species)
  group <- as.character(data_permutation$group)
  freq_rand <- sample(data_permutation$frequency, size=nrow(data_permutation), replace=F)
  data_by_species <- as.data.frame(cbind(species,group,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  data_by_species$freq_rand <- as.numeric(data_by_species$freq_rand) # make sure "freq_rand" is actually a number (and not a character)

  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_by_species)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied") 

  # I need to add all of the columns and make them 0s first 
  data_by_species[,4:(3+numb_lakes)] <- NA  
  data_by_species[,4:(3+numb_lakes)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes
  colnames(data_by_species)[4:(3+numb_lakes)] <- seq(1,numb_lakes)

  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_by_species)){
    lakes_occupied <- sample(seq(1,numb_lakes,1), size=data_by_species$freq_rand[i], replace=F)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied)){
      col_number <- lakes_occupied[j]
      data_by_species[i,(col_number+3)] <- 1      
    }  
  } 
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 

  data_by_lake <- data_by_species
  # save the column of species names  
  list_of_species <- data_by_lake$species 
  # remove things that you don't need 
  data_by_lake$species <- NULL
  data_by_lake$group <- NULL
  data_by_lake$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake <- t(data_by_lake)
  
  # convert it back to a data frame 
  data_by_lake <- as.data.frame(data_by_lake)

  # add back the column names 
  colnames(data_by_lake) <- list_of_species
  
  # for each water body, determine the functional groups present 
  data_by_lake$emergent_richness <- rowSums(data_by_lake[,1:40])
  data_by_lake$floating_richness <- rowSums(data_by_lake[,41:46])
  data_by_lake$lily_richness <- rowSums(data_by_lake[,47:54])
  data_by_lake$submerged_richness <- rowSums(data_by_lake[,55:123])
  
  # add water body "names"
  data_by_lake <- cbind(seq(1,nrow(data_by_lake),1),data_by_lake)
  colnames(data_by_lake)[1] <- "lake"

  # for each functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  permutation_results$emergent[k] <- (sum(data_by_lake$emergent_richness==0) / nrow(data_by_lake))
  permutation_results$floating[k] <- (sum(data_by_lake$floating_richness==0) / nrow(data_by_lake))
  permutation_results$lily[k] <- (sum(data_by_lake$lily_richness==0) / nrow(data_by_lake))
  permutation_results$submerged[k] <- (sum(data_by_lake$submerged_richness==0) / nrow(data_by_lake))

}

# plot the random distribution and the observed % water bodies without each functional group 
hist_without_emergent <- ggplot(permutation_results, aes(x=emergent)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_emergent <- hist_without_emergent + xlab("Proportion water bodies without emergent")
hist_without_emergent <- hist_without_emergent + geom_vline(xintercept=0.22159, colour="red",linetype="longdash")
hist_without_emergent
ggsave("permutation_hist_without_emergent.jpg",hist_without_emergent,height=8,width=8)


hist_without_floating <- ggplot(permutation_results, aes(x=floating)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_floating <- hist_without_floating + xlab("Expected proportion water bodies without floating plants")
hist_without_floating <- hist_without_floating + ylab("Frequency")
hist_without_floating <- hist_without_floating + geom_vline(xintercept=0.5910, colour="red",linetype="longdash")
hist_without_floating <- hist_without_floating + xlim(0,1)
hist_without_floating
ggsave("permutation_hist_without_floating.jpg",hist_without_floating,height=8,width=8)

hist_without_lily <- ggplot(permutation_results, aes(x=lily)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_lily <- hist_without_lily + xlab("Proportion water bodies without lily")
hist_without_lily <- hist_without_lily + geom_vline(xintercept=0.25568, colour="red",linetype="longdash")
hist_without_lily
ggsave("permutation_hist_without_lily.jpg",hist_without_lily,height=8,width=8)

hist_without_submerged <- ggplot(permutation_results, aes(x=submerged)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_submerged <- hist_without_submerged + xlab("Proportion water bodies without submerged")
hist_without_submerged <- hist_without_submerged + geom_vline(xintercept=0.03409, colour="red",linetype="longdash")
hist_without_submerged
ggsave("permutation_hist_without_submerged.jpg",hist_without_submerged,height=8,width=8)



############################### 
# Permutation                 #
# Input: # of spp. in group   #
# Output: prop. absences      #
###############################
# you're going to sample # occurences from this distribution
data_permutation$frequency

# enter your number of lakes & the number of iterations to do 
numb_lakes <- 176
numb_iterations <- 2000 
numb_species <- 24

# set-up the data frame to hold your results 
permutation_results_2 <- matrix(data=0, nrow=numb_iterations, ncol=1)
permutation_results_2 <- as.data.frame(permutation_results_2)
colnames(permutation_results_2) <- "prop_absence"

# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- seq(1,numb_species,1)
  
  freq_rand <- sample(data_permutation$frequency, size=numb_species, replace=F)  
  
  data_by_species <- as.data.frame(cbind(species,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  
  data_by_species$freq_rand <- as.numeric(data_by_species$freq_rand) # make sure "freq_rand" is actually a number (and not a character)
  
  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_by_species)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied") 
  
  # I need to add all of the columns and make them 0s first 
  data_by_species[,3:(2+numb_lakes)] <- NA  
  data_by_species[,3:(2+numb_lakes)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes
  colnames(data_by_species)[3:(2+numb_lakes)] <- seq(1,numb_lakes)
  
  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_by_species)){
    
    # for each species, determine the vector of the number ids of lakes that are occupied
    lakes_occupied <- sample(seq(1,numb_lakes,1), size=data_by_species$freq_rand[i], replace=FALSE)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied)){
      col_number <- lakes_occupied[j]
      data_by_species[i,(col_number+2)] <- 1      
    }  
  } 
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 
  
  data_by_lake <- data_by_species
  # save the column of species names  
  list_of_species <- data_by_lake$species 
  # remove things that you don't need 
  data_by_lake$species <- NULL
  data_by_lake$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake <- t(data_by_lake)
  
  # convert it back to a data frame 
  data_by_lake <- as.data.frame(data_by_lake)
  
  # add back the column names 
  colnames(data_by_lake) <- list_of_species
  
  # for each water body, determine the functional groups present 
  data_by_lake$species_richness <- rowSums(data_by_lake)
    
  # add water body "names"
  data_by_lake <- cbind(seq(1,nrow(data_by_lake),1),data_by_lake)
  colnames(data_by_lake)[1] <- "lake"
  
  # for this functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  permutation_results_2$prop_absence[k] <- (sum(data_by_lake$species_richness==0) / nrow(data_by_lake))
  
}

assign(paste("permutation_results_",numb_species,"_species",sep=""),permutation_results_2)

hist_without <- ggplot(permutation_results_2, aes(x=prop_absence)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without <- hist_without + xlab("Expected proportion water bodies without floating plants")
hist_without <- hist_without + ylab("Frequency")
hist_without <- hist_without + xlim(0,1)
hist_without
ggsave(paste("permutation_hist_without ",numb_species,"_species.jpg",sep=""),hist_without,height=8,width=8)


hist_without_1 <- ggplot(permutation_results_1_species, aes(x=prop_absence)) + geom_histogram(colour="black",fill="grey") + theme_classic(base_size=18)
hist_without_1 <- hist_without_1 + ylab("Frequency")
hist_without_1 <- hist_without_1 + xlab(" ")
hist_without_1 <- hist_without_1 + xlim(0,1)
hist_without_1 <- hist_without_1 + ggtitle("species pool = 1")
hist_without_1 <- hist_without_1 + geom_vline(xintercept=mean(permutation_results_1_species$prop_absence), colour="black",linetype="longdash",size=1)
hist_without_1 <- hist_without_1 + annotate("text",label="a)",x=0.015,y=990,size=6)
hist_without_1

hist_without_6 <- ggplot(permutation_results_6_species, aes(x=prop_absence)) + geom_histogram(colour="black",fill="grey") + theme_classic(base_size=18)
hist_without_6 <- hist_without_6 + xlab(" ")
hist_without_6 <- hist_without_6 + xlim(0,1)
hist_without_6 <- hist_without_6 + ylab(NULL)
hist_without_6 <- hist_without_6 + ggtitle("species pool = 6")
hist_without_6 <- hist_without_6 + geom_vline(xintercept=0.5910, colour="red",linetype="longdash",size=1)
hist_without_6 <- hist_without_6 + geom_vline(xintercept=mean(permutation_results_6_species$prop_absence)+0.005, colour="black",linetype="longdash",size=1)
hist_without_6 <- hist_without_6 + annotate("text",label="b)",x=0.015,y=150,size=6)
hist_without_6

hist_without_12 <- ggplot(permutation_results_12_species, aes(x=prop_absence)) + geom_histogram(colour="black",fill="grey") + theme_classic(base_size=18)
hist_without_12 <- hist_without_12 + xlim(0,1)
hist_without_12 <- hist_without_12 + xlab(" ")
hist_without_12 <- hist_without_12 + ylab(NULL)
hist_without_12 <- hist_without_12 + ggtitle("species pool = 12")
hist_without_12 <- hist_without_12 + geom_vline(xintercept=mean(permutation_results_12_species$prop_absence), colour="black",linetype="longdash",size=1)
hist_without_12 <- hist_without_12 + annotate("text",label="c)",x=0.015,y=225,size=6)
hist_without_12

hist_without_24 <- ggplot(permutation_results_24_species, aes(x=prop_absence)) + geom_histogram(colour="black",fill="grey") + theme_classic(base_size=18)
hist_without_24 <- hist_without_24 + xlim(0,1)
hist_without_24 <- hist_without_24 + xlab(" ")
hist_without_24 <- hist_without_24 + ylab(NULL)
hist_without_24 <- hist_without_24 + ggtitle("species pool = 24")
hist_without_24 <- hist_without_24 + geom_vline(xintercept=mean(permutation_results_24_species$prop_absence), colour="black",linetype="longdash",size=1)
hist_without_24 <- hist_without_24 + annotate("text",label="d)",x=0.01,y=810,size=6)
hist_without_24

library(gridExtra)
hist_without_combined <- arrangeGrob(hist_without_1,hist_without_6,hist_without_12,hist_without_24,ncol=4)
hist_without_combined
ggsave(paste("permutation_hist_without - diff_numb_species.jpg",sep=""),hist_without_combined,height=4,width=16)


#####################
# Calculate 95% CIs #
#####################
sort(permutation_results_1_species[,1])[0.05*length(permutation_results_1_species[,1])]
sort(permutation_results_1_species[,1])[0.95*length(permutation_results_1_species[,1])]

sort(permutation_results_6_species[,1])[0.05*length(permutation_results_6_species[,1])]
sort(permutation_results_6_species[,1])[0.95*length(permutation_results_6_species[,1])]

sort(permutation_results_12_species[,1])[0.05*length(permutation_results_12_species[,1])]
sort(permutation_results_12_species[,1])[0.95*length(permutation_results_12_species[,1])]

sort(permutation_results_24_species[,1])[0.05*length(permutation_results_24_species[,1])]
sort(permutation_results_24_species[,1])[0.95*length(permutation_results_24_species[,1])]
