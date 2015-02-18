#####################################
# Permutation test                  #    
# Floating plants only              #
# Other regions                     #
# MJM 10/1/2014                     #
#####################################
# Question: 
# Is the freq. of occurence/absence of a functional group different than expected based on species richness of the functional group in the region? 
# More specificially: 
# Is the freq. of occurence of the floating plant functional group just a function of their low regional richness?

library(ggplot2)

# 5(?) regions 
# Bornette - France 
# Edvardsen - Norway 
# Vestergaard - Denmark
# WA 


# import and set-up your data frame 
data_perm_bornette <- read.csv("bornette.csv")
data_perm_bornette$Label <- NULL # remove variables that you don't need 
data_perm_bornette$FP. <- NULL
data_perm_bornette$Group <- NULL 

data_perm_edvardsen <- read.csv("edvardsen_okland.csv")
data_perm_edvardsen$Label <- NULL # remove variables that you don't need 
data_perm_edvardsen$FP. <- NULL
data_perm_edvardsen$Group <- NULL

data_perm_vestergaard <- read.csv("vestergaard_sand-jensen.csv")
data_perm_vestergaard$Label <- NULL # remove variables that you don't need 
data_perm_vestergaard$FP. <- NULL
data_perm_vestergaard$Group <- NULL
data_perm_vestergaard$Percent <- NULL

data_perm_toivonen <- read.csv("toivonen.csv")
data_perm_toivonen$Label <- NULL # remove variables that you don't need 
data_perm_toivonen$FP. <- NULL
data_perm_toivonen$grouping1 <- NULL
data_perm_toivonen$grouping2 <- NULL
data_perm_toivonen$code <- NULL
data_perm_toivonen$Rank <- NULL
data_perm_toivonen$Percent <- NULL

# enter your number of lakes & the number of iterations to do 
numb_lakes_bornette <- 23
numb_lakes_edvardsen <- 64
numb_lakes_vestergaard <- 82
numb_lakes_toivonen <- 57
numb_iterations <- 2000

# set-up the data frame to hold your results 
perm_results_bornette <- matrix(data=0, nrow=numb_iterations, ncol=4)
perm_results_bornette <- as.data.frame(perm_results_bornette)
colnames(perm_results_bornette) <- c("emergent","floating","lily","submerged")

perm_results_edvardsen <- matrix(data=0, nrow=numb_iterations, ncol=4)
perm_results_edvardsen <- as.data.frame(perm_results_edvardsen)
colnames(perm_results_edvardsen) <- c("emergent","floating","lily","submerged")

perm_results_vestergaard <- matrix(data=0, nrow=numb_iterations, ncol=4)
perm_results_vestergaard <- as.data.frame(perm_results_vestergaard)
colnames(perm_results_vestergaard) <- c("emergent","floating","lily","submerged")

perm_results_toivonen <- matrix(data=0, nrow=numb_iterations, ncol=4)
perm_results_toivonen <- as.data.frame(perm_results_toivonen)
colnames(perm_results_toivonen) <- c("emergent","floating","lily","submerged")

################################################
# data_permutation -> data_perm_bornette       #
# data_by_species -> data_species_bornette     #
# lakes_occupied -> lakes_occupied_bornette    #
# data_by_lake -> data_by_lake_bornette        #
# permutation_results -> perm_results_bornette #
# numb_lakes -> numb_lakes_bornette            #
################################################
# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- as.character(data_perm_bornette$Species)
  group <- as.character(data_perm_bornette$group)
  freq_rand <- sample(data_perm_bornette$Frequency, size=nrow(data_perm_bornette), replace=F)
  data_species_bornette <- as.data.frame(cbind(species,group,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  data_species_bornette$freq_rand <- as.numeric(data_species_bornette$freq_rand) # make sure "freq_rand" is actually a number (and not a character)

  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_species_bornette)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied_bornette") 

  # I need to add all of the columns and make them 0s first 
  data_species_bornette[,4:(3+numb_lakes_bornette)] <- NA  
  data_species_bornette[,4:(3+numb_lakes_bornette)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes_bornette
  colnames(data_species_bornette)[4:(3+numb_lakes_bornette)] <- seq(1,numb_lakes_bornette)

  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_species_bornette)){
    lakes_occupied_bornette <- sample(seq(1,numb_lakes_bornette,1), size=data_species_bornette$freq_rand[i], replace=F)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied_bornette)){
      col_number <- lakes_occupied_bornette[j]
      data_species_bornette[i,(col_number+3)] <- 1      
    }  
  } 
  
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 

  data_by_lake_bornette <- data_species_bornette
  # save the column of species names  
  list_of_species <- data_by_lake_bornette$species 
  # remove things that you don't need 
  data_by_lake_bornette$species <- NULL
  data_by_lake_bornette$group <- NULL
  data_by_lake_bornette$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake_bornette <- t(data_by_lake_bornette)
  
  # convert it back to a data frame 
  data_by_lake_bornette <- as.data.frame(data_by_lake_bornette)

  # add back the column names 
  colnames(data_by_lake_bornette) <- list_of_species
  
  #################################################
  # CHANGE THIS SECTION FOR EACH DIFFERENT REGION #
  #################################################
  # for each water body, determine the functional groups present 
  data_by_lake_bornette$emergent_richness <- rowSums(data_by_lake_bornette[,1:50])
  data_by_lake_bornette$floating_richness <- rowSums(data_by_lake_bornette[,51:53])
  data_by_lake_bornette$lily_richness <- rowSums(data_by_lake_bornette[,54:56])
  data_by_lake_bornette$submerged_richness <- rowSums(data_by_lake_bornette[,57:83])
  
  # add water body "names"
  data_by_lake_bornette <- cbind(seq(1,nrow(data_by_lake_bornette),1),data_by_lake_bornette)
  colnames(data_by_lake_bornette)[1] <- "lake"

  # for each functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  perm_results_bornette$emergent[k] <- (sum(data_by_lake_bornette$emergent_richness==0) / nrow(data_by_lake_bornette))
  perm_results_bornette$floating[k] <- (sum(data_by_lake_bornette$floating_richness==0) / nrow(data_by_lake_bornette))
  perm_results_bornette$lily[k] <- (sum(data_by_lake_bornette$lily_richness==0) / nrow(data_by_lake_bornette))
  perm_results_bornette$submerged[k] <- (sum(data_by_lake_bornette$submerged_richness==0) / nrow(data_by_lake_bornette))
}


# plot the random distribution and the observed % water bodies without each functional group 
hist_without_emergent_bornette <- ggplot(perm_results_bornette, aes(x=emergent)) 
hist_without_emergent_bornette <- hist_without_emergent_bornette + geom_histogram(colour="black",fill="white") 
hist_without_emergent_bornette <- hist_without_emergent_bornette + theme_classic(base_size=18)
hist_without_emergent_bornette <- hist_without_emergent_bornette + xlab("Proportion water bodies without emergent")
hist_without_emergent_bornette <- hist_without_emergent_bornette + geom_vline(xintercept=0, colour="red",linetype="longdash")
hist_without_emergent_bornette
ggsave("permutation_hist_without_emergent_bornette.jpg",hist_without_emergent_bornette,height=8,width=8)

hist_without_floating_bornette <- ggplot(perm_results_bornette, aes(x=floating)) 
hist_without_floating_bornette <- hist_without_floating_bornette + geom_histogram(colour="black",fill="white") 
hist_without_floating_bornette <- hist_without_floating_bornette + theme_classic(base_size=18)
hist_without_floating_bornette <- hist_without_floating_bornette + xlab("Proportion water bodies without floating")
hist_without_floating_bornette <- hist_without_floating_bornette + geom_vline(xintercept=0.7391, colour="red",linetype="longdash")
hist_without_floating_bornette
ggsave("permutation_hist_without_floating_bornette.jpg",hist_without_floating_bornette,height=8,width=8)

hist_without_lily_bornette <- ggplot(perm_results_bornette, aes(x=lily)) 
hist_without_lily_bornette <- hist_without_lily_bornette + geom_histogram(colour="black",fill="white") 
hist_without_lily_bornette <- hist_without_lily_bornette + theme_classic(base_size=18)
hist_without_lily_bornette <- hist_without_lily_bornette + xlab("Proportion water bodies without lily")
hist_without_lily_bornette <- hist_without_lily_bornette + geom_vline(xintercept=0.6087, colour="red",linetype="longdash")
hist_without_lily_bornette
ggsave("permutation_hist_without_lily_bornette.jpg",hist_without_lily_bornette,height=8,width=8)

hist_without_submerged_bornette <- ggplot(perm_results_bornette, aes(x=submerged)) 
hist_without_submerged_bornette <- hist_without_submerged_bornette + geom_histogram(colour="black",fill="white") 
hist_without_submerged_bornette <- hist_without_submerged_bornette + theme_classic(base_size=18)
hist_without_submerged_bornette <- hist_without_submerged_bornette + xlab("Proportion water bodies without submerged")
hist_without_submerged_bornette <- hist_without_submerged_bornette + geom_vline(xintercept=0.0, colour="red",linetype="longdash")
hist_without_submerged_bornette
ggsave("permutation_hist_without_submerged_bornette.jpg",hist_without_submerged_bornette,height=8,width=8)





#################################################
# data_permutation -> data_perm_edvardsen       #
# data_by_species -> data_species_edvardsen     #
# lakes_occupied -> lakes_occupied_edvardsen    #
# data_by_lake -> data_by_lake_edvardsen        #
# permutation_results -> perm_results_edvardsen #
# numb_lakes -> numb_lakes_edvardsen            #
#################################################
# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- as.character(data_perm_edvardsen$Species)
  group <- as.character(data_perm_edvardsen$group)
  freq_rand <- sample(data_perm_edvardsen$Frequency, size=nrow(data_perm_edvardsen), replace=F)
  data_species_edvardsen <- as.data.frame(cbind(species,group,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  data_species_edvardsen$freq_rand <- as.numeric(data_species_edvardsen$freq_rand) # make sure "freq_rand" is actually a number (and not a character)
  
  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_species_edvardsen)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied_edvardsen") 
  
  # I need to add all of the columns and make them 0s first 
  data_species_edvardsen[,4:(3+numb_lakes_edvardsen)] <- NA  
  data_species_edvardsen[,4:(3+numb_lakes_edvardsen)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes_edvardsen
  colnames(data_species_edvardsen)[4:(3+numb_lakes_edvardsen)] <- seq(1,numb_lakes_edvardsen)
  
  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_species_edvardsen)){
    lakes_occupied_edvardsen <- sample(seq(1,numb_lakes_edvardsen,1), size=data_species_edvardsen$freq_rand[i], replace=F)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied_edvardsen)){
      col_number <- lakes_occupied_edvardsen[j]
      data_species_edvardsen[i,(col_number+3)] <- 1      
    }  
  } 
  
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 
  
  data_by_lake_edvardsen <- data_species_edvardsen
  # save the column of species names  
  list_of_species <- data_by_lake_edvardsen$species 
  # remove things that you don't need 
  data_by_lake_edvardsen$species <- NULL
  data_by_lake_edvardsen$group <- NULL
  data_by_lake_edvardsen$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake_edvardsen <- t(data_by_lake_edvardsen)
  
  # convert it back to a data frame 
  data_by_lake_edvardsen <- as.data.frame(data_by_lake_edvardsen)
  
  # add back the column names 
  colnames(data_by_lake_edvardsen) <- list_of_species
  
  #################################################
  # CHANGE THIS SECTION FOR EACH DIFFERENT REGION #
  #################################################
  # for each water body, determine the functional groups present 
  data_by_lake_edvardsen$emergent_richness <- rowSums(data_by_lake_edvardsen[,1:30])
  data_by_lake_edvardsen$floating_richness <- rowSums(data_by_lake_edvardsen[,31:33])
  data_by_lake_edvardsen$lily_richness <- rowSums(data_by_lake_edvardsen[,34:35])
  data_by_lake_edvardsen$submerged_richness <- rowSums(data_by_lake_edvardsen[,36:47])
  
  # add water body "names"
  data_by_lake_edvardsen <- cbind(seq(1,nrow(data_by_lake_edvardsen),1),data_by_lake_edvardsen)
  colnames(data_by_lake_edvardsen)[1] <- "lake"
  
  # for each functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  perm_results_edvardsen$emergent[k] <- (sum(data_by_lake_edvardsen$emergent_richness==0) / nrow(data_by_lake_edvardsen))
  perm_results_edvardsen$floating[k] <- (sum(data_by_lake_edvardsen$floating_richness==0) / nrow(data_by_lake_edvardsen))
  perm_results_edvardsen$lily[k] <- (sum(data_by_lake_edvardsen$lily_richness==0) / nrow(data_by_lake_edvardsen))
  perm_results_edvardsen$submerged[k] <- (sum(data_by_lake_edvardsen$submerged_richness==0) / nrow(data_by_lake_edvardsen))
}


# plot the random distribution and the observed % water bodies without each functional group 
hist_without_emergent_edvardsen <- ggplot(perm_results_edvardsen, aes(x=emergent)) 
hist_without_emergent_edvardsen <- hist_without_emergent_edvardsen + geom_histogram(colour="black",fill="white") 
hist_without_emergent_edvardsen <- hist_without_emergent_edvardsen + theme_classic(base_size=18)
hist_without_emergent_edvardsen <- hist_without_emergent_edvardsen + xlab("Proportion water bodies without emergent")
#hist_without_emergent_edvardsen <- hist_without_emergent_edvardsen + geom_vline(xintercept=0, colour="red",linetype="longdash")
hist_without_emergent_edvardsen
ggsave("permutation_hist_without_emergent_edvardsen.jpg",hist_without_emergent_edvardsen,height=8,width=8)

hist_without_floating_edvardsen <- ggplot(perm_results_edvardsen, aes(x=floating)) 
hist_without_floating_edvardsen <- hist_without_floating_edvardsen + geom_histogram(colour="black",fill="white") 
hist_without_floating_edvardsen <- hist_without_floating_edvardsen + theme_classic(base_size=18)
hist_without_floating_edvardsen <- hist_without_floating_edvardsen + xlab("Proportion water bodies without floating")
hist_without_floating_edvardsen <- hist_without_floating_edvardsen + geom_vline(xintercept=0.2031, colour="red",linetype="longdash")
hist_without_floating_edvardsen
ggsave("permutation_hist_without_floating_edvardsen.jpg",hist_without_floating_edvardsen,height=8,width=8)

hist_without_lily_edvardsen <- ggplot(perm_results_edvardsen, aes(x=lily)) 
hist_without_lily_edvardsen <- hist_without_lily_edvardsen + geom_histogram(colour="black",fill="white") 
hist_without_lily_edvardsen <- hist_without_lily_edvardsen + theme_classic(base_size=18)
hist_without_lily_edvardsen <- hist_without_lily_edvardsen + xlab("Proportion water bodies without lily")
hist_without_lily_edvardsen <- hist_without_lily_edvardsen + geom_vline(xintercept=0.375, colour="red",linetype="longdash")
hist_without_lily_edvardsen
ggsave("permutation_hist_without_lily_edvardsen.jpg",hist_without_lily_edvardsen,height=8,width=8)

hist_without_submerged_edvardsen <- ggplot(perm_results_edvardsen, aes(x=submerged)) 
hist_without_submerged_edvardsen <- hist_without_submerged_edvardsen + geom_histogram(colour="black",fill="white") 
hist_without_submerged_edvardsen <- hist_without_submerged_edvardsen + theme_classic(base_size=18)
hist_without_submerged_edvardsen <- hist_without_submerged_edvardsen + xlab("Proportion water bodies without submerged")
hist_without_submerged_edvardsen <- hist_without_submerged_edvardsen + geom_vline(xintercept=0.3125, colour="red",linetype="longdash")
hist_without_submerged_edvardsen
ggsave("permutation_hist_without_submerged_edvardsen.jpg",hist_without_submerged_edvardsen,height=8,width=8)







###################################################
# data_permutation -> data_perm_vestergaard       #
# data_by_species -> data_species_vestergaard     #
# lakes_occupied -> lakes_occupied_vestergaard    #
# data_by_lake -> data_by_lake_vestergaard        #
# permutation_results -> perm_results_vestergaard #
# numb_lakes -> numb_lakes_vestergaard            #
###################################################
# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- as.character(data_perm_vestergaard$Species)
  group <- as.character(data_perm_vestergaard$group)
  freq_rand <- sample(data_perm_vestergaard$Frequency, size=nrow(data_perm_vestergaard), replace=F)
  data_species_vestergaard <- as.data.frame(cbind(species,group,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  data_species_vestergaard$freq_rand <- as.numeric(data_species_vestergaard$freq_rand) # make sure "freq_rand" is actually a number (and not a character)
  
  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_species_vestergaard)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied_vestergaard") 
  
  # I need to add all of the columns and make them 0s first 
  data_species_vestergaard[,4:(3+numb_lakes_vestergaard)] <- NA  
  data_species_vestergaard[,4:(3+numb_lakes_vestergaard)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes_vestergaard
  colnames(data_species_vestergaard)[4:(3+numb_lakes_vestergaard)] <- seq(1,numb_lakes_vestergaard)
  
  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_species_vestergaard)){
    lakes_occupied_vestergaard <- sample(seq(1,numb_lakes_vestergaard,1), size=data_species_vestergaard$freq_rand[i], replace=F)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied_vestergaard)){
      col_number <- lakes_occupied_vestergaard[j]
      data_species_vestergaard[i,(col_number+3)] <- 1      
    }  
  } 
  
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 
  
  data_by_lake_vestergaard <- data_species_vestergaard
  # save the column of species names  
  list_of_species <- data_by_lake_vestergaard$species 
  # remove things that you don't need 
  data_by_lake_vestergaard$species <- NULL
  data_by_lake_vestergaard$group <- NULL
  data_by_lake_vestergaard$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake_vestergaard <- t(data_by_lake_vestergaard)
  
  # convert it back to a data frame 
  data_by_lake_vestergaard <- as.data.frame(data_by_lake_vestergaard)
  
  # add back the column names 
  colnames(data_by_lake_vestergaard) <- list_of_species
  
  #################################################
  # CHANGE THIS SECTION FOR EACH DIFFERENT REGION #
  #################################################
  # for each water body, determine the functional groups present 
  data_by_lake_vestergaard$emergent_richness <- rowSums(data_by_lake_vestergaard[,1:19])
  data_by_lake_vestergaard$floating_richness <- data_by_lake_vestergaard[,20]
  data_by_lake_vestergaard$lily_richness <- data_by_lake_vestergaard[,21]
  data_by_lake_vestergaard$submerged_richness <- rowSums(data_by_lake_vestergaard[,22:103])
  
  # add water body "names"
  data_by_lake_vestergaard <- cbind(seq(1,nrow(data_by_lake_vestergaard),1),data_by_lake_vestergaard)
  colnames(data_by_lake_vestergaard)[1] <- "lake"
  
  # for each functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  perm_results_vestergaard$emergent[k] <- (sum(data_by_lake_vestergaard$emergent_richness==0) / nrow(data_by_lake_vestergaard))
  perm_results_vestergaard$floating[k] <- (sum(data_by_lake_vestergaard$floating_richness==0) / nrow(data_by_lake_vestergaard))
  perm_results_vestergaard$lily[k] <- (sum(data_by_lake_vestergaard$lily_richness==0) / nrow(data_by_lake_vestergaard))
  perm_results_vestergaard$submerged[k] <- (sum(data_by_lake_vestergaard$submerged_richness==0) / nrow(data_by_lake_vestergaard))
}


# plot the random distribution and the observed % water bodies without each functional group 
hist_without_emergent_vestergaard <- ggplot(perm_results_vestergaard, aes(x=emergent)) 
hist_without_emergent_vestergaard <- hist_without_emergent_vestergaard + geom_histogram(colour="black",fill="white") 
hist_without_emergent_vestergaard <- hist_without_emergent_vestergaard + theme_classic(base_size=18)
hist_without_emergent_vestergaard <- hist_without_emergent_vestergaard + xlab("Proportion water bodies without emergent")
#hist_without_emergent_vestergaard <- hist_without_emergent_vestergaard + geom_vline(xintercept=0, colour="red",linetype="longdash")
hist_without_emergent_vestergaard
ggsave("permutation_hist_without_emergent_vestergaard.jpg",hist_without_emergent_vestergaard,height=8,width=8)

hist_without_floating_vestergaard <- ggplot(perm_results_vestergaard, aes(x=floating)) 
hist_without_floating_vestergaard <- hist_without_floating_vestergaard + geom_histogram(colour="black",fill="white") 
hist_without_floating_vestergaard <- hist_without_floating_vestergaard + theme_classic(base_size=18)
hist_without_floating_vestergaard <- hist_without_floating_vestergaard + xlab("Proportion water bodies without floating")
hist_without_floating_vestergaard <- hist_without_floating_vestergaard + geom_vline(xintercept=0.817, colour="red",linetype="longdash")
hist_without_floating_vestergaard
ggsave("permutation_hist_without_floating_vestergaard.jpg",hist_without_floating_vestergaard,height=8,width=8)

hist_without_lily_vestergaard <- ggplot(perm_results_vestergaard, aes(x=lily)) 
hist_without_lily_vestergaard <- hist_without_lily_vestergaard + geom_histogram(colour="black",fill="white") 
hist_without_lily_vestergaard <- hist_without_lily_vestergaard + theme_classic(base_size=18)
hist_without_lily_vestergaard <- hist_without_lily_vestergaard + xlab("Proportion water bodies without lily")
#hist_without_lily_vestergaard <- hist_without_lily_vestergaard + geom_vline(xintercept=0.6087, colour="red",linetype="longdash")
hist_without_lily_vestergaard
ggsave("permutation_hist_without_lily_vestergaard.jpg",hist_without_lily_vestergaard,height=8,width=8)

hist_without_submerged_vestergaard <- ggplot(perm_results_vestergaard, aes(x=submerged)) 
hist_without_submerged_vestergaard <- hist_without_submerged_vestergaard + geom_histogram(colour="black",fill="white") 
hist_without_submerged_vestergaard <- hist_without_submerged_vestergaard + theme_classic(base_size=18)
hist_without_submerged_vestergaard <- hist_without_submerged_vestergaard + xlab("Proportion water bodies without submerged")
#hist_without_submerged_vestergaard <- hist_without_submerged_vestergaard + geom_vline(xintercept=0.0, colour="red",linetype="longdash")
hist_without_submerged_vestergaard
ggsave("permutation_hist_without_submerged_vestergaard.jpg",hist_without_submerged_vestergaard,height=8,width=8)








#################################################
# data_permutation -> data_perm_toivonen       #
# data_by_species -> data_species_toivonen     #
# lakes_occupied -> lakes_occupied_toivonen    #
# data_by_lake -> data_by_lake_toivonen        #
# permutation_results -> perm_results_toivonen #
# numb_lakes -> numb_lakes_toivonen            #
#################################################
# repeat this numb_iterations times 
for(k in 1:numb_iterations){
  
  ##############################################
  # for each species, randomly draw freq.      #
  # from distribution of all observed freqs    #
  # (no replacement)                           #
  ##############################################
  species <- as.character(data_perm_toivonen$Species)
  group <- as.character(data_perm_toivonen$group)
  freq_rand <- sample(data_perm_toivonen$Frequency, size=nrow(data_perm_toivonen), replace=F)
  data_species_toivonen <- as.data.frame(cbind(species,group,freq_rand)) # add "species", "group" and "freq_rand" back together in a data frame 
  data_species_toivonen$freq_rand <- as.numeric(data_species_toivonen$freq_rand) # make sure "freq_rand" is actually a number (and not a character)
  
  ##############################################
  # for each species, randomly assign to lakes #
  # based on the randomly drawn frequency      #
  ##############################################
  # i will index each species (each row of data_species_toivonen)
  # j will index each lake id # that each species is found in (stored in the vector "lakes_occupied_toivonen") 
  
  # I need to add all of the columns and make them 0s first 
  data_species_toivonen[,4:(3+numb_lakes_toivonen)] <- NA  
  data_species_toivonen[,4:(3+numb_lakes_toivonen)] <- 0 
  
  # give them column names that are just a sequence from 1 to numb_lakes_toivonen
  colnames(data_species_toivonen)[4:(3+numb_lakes_toivonen)] <- seq(1,numb_lakes_toivonen)
  
  # loop through all species (rows) and sample water body IDs based on freq_rand
  for (i in 1:nrow(data_species_toivonen)){
    lakes_occupied_toivonen <- sample(seq(1,numb_lakes_toivonen,1), size=data_species_toivonen$freq_rand[i], replace=F)
    
    # loop through all of the water body IDS (for each species) and assign a 1 to those columns 
    # use the water body ID to index the data frame 
    for (j in 1:length(lakes_occupied_toivonen)){
      col_number <- lakes_occupied_toivonen[j]
      data_species_toivonen[i,(col_number+3)] <- 1      
    }  
  } 
  
  # for each water body, determine the species present   
  # try converting this data frame into a matrix then inverting it 
  
  data_by_lake_toivonen <- data_species_toivonen
  # save the column of species names  
  list_of_species <- data_by_lake_toivonen$species 
  # remove things that you don't need 
  data_by_lake_toivonen$species <- NULL
  data_by_lake_toivonen$group <- NULL
  data_by_lake_toivonen$freq_rand <- NULL
  
  # transpose the matrix 
  data_by_lake_toivonen <- t(data_by_lake_toivonen)
  
  # convert it back to a data frame 
  data_by_lake_toivonen <- as.data.frame(data_by_lake_toivonen)
  
  # add back the column names 
  colnames(data_by_lake_toivonen) <- list_of_species
  
  #################################################
  # CHANGE THIS SECTION FOR EACH DIFFERENT REGION #
  #################################################
  # for each water body, determine the functional groups present 
  data_by_lake_toivonen$emergent_richness <- rowSums(data_by_lake_toivonen[,1:38])
  data_by_lake_toivonen$floating_richness <- rowSums(data_by_lake_toivonen[,39:44])
  data_by_lake_toivonen$lily_richness <- rowSums(data_by_lake_toivonen[,45:46])
  data_by_lake_toivonen$submerged_richness <- rowSums(data_by_lake_toivonen[,47:91])
  
  # add water body "names"
  data_by_lake_toivonen <- cbind(seq(1,nrow(data_by_lake_toivonen),1),data_by_lake_toivonen)
  colnames(data_by_lake_toivonen)[1] <- "lake"
  
  # for each functional group, calculate the % water bodies without the functional group 
  # the % of water bodies w/o each functional group 
  perm_results_toivonen$emergent[k] <- (sum(data_by_lake_toivonen$emergent_richness==0) / nrow(data_by_lake_toivonen))
  perm_results_toivonen$floating[k] <- (sum(data_by_lake_toivonen$floating_richness==0) / nrow(data_by_lake_toivonen))
  perm_results_toivonen$lily[k] <- (sum(data_by_lake_toivonen$lily_richness==0) / nrow(data_by_lake_toivonen))
  perm_results_toivonen$submerged[k] <- (sum(data_by_lake_toivonen$submerged_richness==0) / nrow(data_by_lake_toivonen))
}


# plot the random distribution and the observed % water bodies without each functional group 
hist_without_emergent_toivonen <- ggplot(perm_results_toivonen, aes(x=emergent)) 
hist_without_emergent_toivonen <- hist_without_emergent_toivonen + geom_histogram(colour="black",fill="white") 
hist_without_emergent_toivonen <- hist_without_emergent_toivonen + theme_classic(base_size=18)
hist_without_emergent_toivonen <- hist_without_emergent_toivonen + xlab("Proportion water bodies without emergent")
#hist_without_emergent_toivonen <- hist_without_emergent_toivonen + geom_vline(xintercept=0, colour="red",linetype="longdash")
hist_without_emergent_toivonen
ggsave("permutation_hist_without_emergent_toivonen.jpg",hist_without_emergent_toivonen,height=8,width=8)

hist_without_floating_toivonen <- ggplot(perm_results_toivonen, aes(x=floating)) 
hist_without_floating_toivonen <- hist_without_floating_toivonen + geom_histogram(colour="black",fill="white") 
hist_without_floating_toivonen <- hist_without_floating_toivonen + theme_classic(base_size=18)
hist_without_floating_toivonen <- hist_without_floating_toivonen + xlab("Proportion water bodies without floating")
hist_without_floating_toivonen <- hist_without_floating_toivonen + geom_vline(xintercept=0.2031, colour="red",linetype="longdash")
hist_without_floating_toivonen
ggsave("permutation_hist_without_floating_toivonen.jpg",hist_without_floating_toivonen,height=8,width=8)

hist_without_lily_toivonen <- ggplot(perm_results_toivonen, aes(x=lily)) 
hist_without_lily_toivonen <- hist_without_lily_toivonen + geom_histogram(colour="black",fill="white") 
hist_without_lily_toivonen <- hist_without_lily_toivonen + theme_classic(base_size=18)
hist_without_lily_toivonen <- hist_without_lily_toivonen + xlab("Proportion water bodies without lily")
hist_without_lily_toivonen <- hist_without_lily_toivonen + geom_vline(xintercept=0.375, colour="red",linetype="longdash")
hist_without_lily_toivonen
ggsave("permutation_hist_without_lily_toivonen.jpg",hist_without_lily_toivonen,height=8,width=8)

hist_without_submerged_toivonen <- ggplot(perm_results_toivonen, aes(x=submerged)) 
hist_without_submerged_toivonen <- hist_without_submerged_toivonen + geom_histogram(colour="black",fill="white") 
hist_without_submerged_toivonen <- hist_without_submerged_toivonen + theme_classic(base_size=18)
hist_without_submerged_toivonen <- hist_without_submerged_toivonen + xlab("Proportion water bodies without submerged")
hist_without_submerged_toivonen <- hist_without_submerged_toivonen + geom_vline(xintercept=0.3125, colour="red",linetype="longdash")
hist_without_submerged_toivonen
ggsave("permutation_hist_without_submerged_toivonen.jpg",hist_without_submerged_toivonen,height=8,width=8)

