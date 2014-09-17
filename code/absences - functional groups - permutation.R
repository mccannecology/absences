#####################################
# Permutation test                  #    
# Aquatic plant functional groups   # 
# CT Agricultural Exp. Station data #
# MJM 9/17/2014                     #
#####################################
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
numb_lakes <- 174
numb_iterations <- 2000

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
hist_without_emergent <- hist_without_emergent + geom_vline(xintercept=0.207, colour="red",linetype="longdash")
hist_without_emergent
ggsave("permutation_hist_without_emergent.jpg",hist_without_emergent,height=8,width=8)


hist_without_floating <- ggplot(permutation_results, aes(x=floating)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_floating <- hist_without_floating + xlab("Proportion water bodies without floating")
hist_without_floating <- hist_without_floating + geom_vline(xintercept=0.586, colour="red",linetype="longdash")
hist_without_floating
ggsave("permutation_hist_without_floating.jpg",hist_without_floating,height=8,width=8)

hist_without_lily <- ggplot(permutation_results, aes(x=lily)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_lily <- hist_without_lily + xlab("Proportion water bodies without lily")
hist_without_lily <- hist_without_lily + geom_vline(xintercept=0.224, colour="red",linetype="longdash")
hist_without_lily
ggsave("permutation_hist_without_lily.jpg",hist_without_lily,height=8,width=8)

hist_without_submerged <- ggplot(permutation_results, aes(x=submerged)) + geom_histogram(colour="black",fill="white") + theme_classic(base_size=18)
hist_without_submerged <- hist_without_submerged + xlab("Proportion water bodies without submerged")
hist_without_submerged <- hist_without_submerged + geom_vline(xintercept=0.028, colour="red",linetype="longdash")
hist_without_submerged
ggsave("permutation_hist_without_submerged.jpg",hist_without_submerged,height=8,width=8)
