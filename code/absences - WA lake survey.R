##################################################
# Washington State Aquatic Plant Surveys         #
# Department of Ecology                          #
# Download:  http://www.ecy.wa.gov/eim/index.htm #
# Download date: 8/14/2014                       #
##################################################

# set new working directory
setwd("C:/Users/Mike/Desktop/Dropbox/absences/other regions/WA")

###############
# import data #
###############
# "details" and "locations" are not very useful 
# "details" are short verbal comments
# details <- read.csv("EIMStudyDetails.csv")
# most of the information in "locations" is missing / not relevant 
# locations <- read.csv("EIMLocationDetails.csv")
# "dataWA" is the good stuff 
dataWA_raw <- read.csv("EIMResults.csv")

# make a separate data frame for manipulating and simplifying 
dataWA <- dataWA_raw

# remove sechhi disk records 
dataWA <- subset(dataWA, dataWA$Result_Method != "SECCHI-L")

#################
# summary stats #
#################
# Number of unique lakes surveyed 
length(unique(dataWA$Location_ID))
nrow(locations) # good the number of rows in "locations" matches 

# Total number of species/genera identified  
length(unique(dataWA$Result_Taxon_TSN))
length(unique(dataWA$Result_Taxon_Name)) # good both match - the text or numeric versions of species IDs 

# Extract year from date 
temp <- NULL 
for (i in 1:nrow(dataWA)) {
  temp[i] <- strsplit(as.character(dataWA$Field_Collection_Start_Date[i]), "/")[[1]][3]
}
dataWA$year <- as.numeric(temp) # assign this to the main data frame 
rm(temp,i) # clean up your junk 

# make a variable "site_year" that combines  the site and year 
dataWA$site_year <- do.call(paste,c(dataWA[c("Location_ID","year")],sep=" "))

# number of unique surveys (counts multiple per water body)
length(unique(dataWA$site_year)) # 1371

# List of unique taxa names 
unique(dataWA$Result_Taxon_Name)

# Number of records for an individual taxa 
nrow(subset(dataWA, dataWA$Result_Taxon_Name == "Alisma"))

# loop throught problematic taxa & count # of records each 
# this will be useful for figuring out if/how to lump species into genera 
temp <- read.csv("list_of_dumb_taxa.csv",header=FALSE)
list_of_species <- as.matrix(temp)
for (i in unique(list_of_species)) {
  print(nrow(subset(dataWA, dataWA$Result_Taxon_Name == i)))
}
rm(i,list_of_species,temp) # clean up your workspace 


################
# reshape data #
################
# Goal: one row per lake survey 

# add a dummy variable for counting up presence 
dataWA$dummy <- 1

# remove any duplicate values
# Result_Taxon_Name appears twice if Result_Taxon_Common_Name is in twice (I don't know why it is)

# Using a package that takes forever 
# library(BiodiversityR)
# community_matrix <- makecommunitydataset(dataWA,row="site_year",column="Result_Taxon_Name",value="dummy")

# using table() from the base package 
dataWA_spmatrix <- table(dataWA$site_year,dataWA$Result_Taxon_Name)
dataWA_spmatrix <- as.matrix(dataWA_spmatrix)

# remove the weird first column of 0s
dataWA_spmatrix <- dataWA_spmatrix[,-1]

# Check the # of rows and colums 
nrow(dataWA_spmatrix) # should match the number of unique surveys 
ncol(dataWA_spmatrix) # should match the number of unique taxa 

# convert any "2"s or greater to 1
dataWA_spmatrix[dataWA_spmatrix>1] <- 1

# check out the top corner of the matrix 
dataWA_spmatrix[1:10,1:10]

write.csv(dataWA_spmatrix,"dataWA_spmatrix.csv")

# convert to a data frame 
dataWA_spmatrix <- as.data.frame(dataWA_spmatrix)

# make the row names a variable 
dataWA_spmatrix$site_year <- rownames(dataWA_spmatrix)

# split site_year into separate columns for site and year 
temp <- strsplit(dataWA_spmatrix$site_year, " ")
dataWA_spmatrix$site <- as.factor(do.call(rbind,temp)[,1])
dataWA_spmatrix$year <- as.numeric(do.call(rbind,temp)[,2])

#############################
# Colonization / Extinction #
# Waterbodies w/ FP         #
# all surveys               #
#############################
# Do I need to subset my data first (and just look at the FP species)?
dataWA_FPonly_ALL <- cbind(dataWA_spmatrix["Azolla"],dataWA_spmatrix["Azolla cristata"],
                           dataWA_spmatrix["Eichhornia crassipes"],dataWA_spmatrix["Lemna minor"],
                           dataWA_spmatrix["Lemna"],dataWA_spmatrix["Lemna trisulca"],
                           dataWA_spmatrix["Spirodela polyrrhiza"],dataWA_spmatrix["Wolffia"],
                           dataWA_spmatrix["Wolffia columbiana"],
                           dataWA_spmatrix["year"],dataWA_spmatrix["site"])


# for each waterbody 
# check if any of the FP species are present
# if so, include those waterbodies (even surveys that did not have any FP)

dataWA_lakes_FP_ever <- NULL

for (i in unique(dataWA_FPonly_ALL$site)){
  
  # get all records (surveys) for a single site 
  temp <- subset(dataWA_FPonly_ALL, dataWA_FPonly_ALL$site == i) 
  
  # check if ANY of the surveys for that lake have ANY of the FP speices 
  
  if (sum(temp["Azolla"]) >0 | sum(temp["Azolla cristata"]) >0 |
        sum(temp["Eichhornia crassipes"]) >0 | sum(temp["Lemna minor"]) >0 | 
        sum(temp["Lemna"]) >0 | sum(temp["Lemna trisulca"]) >0 |
        sum(temp["Spirodela polyrrhiza"]) >0 | sum(temp["Wolffia"]) >0 |
        sum(temp["Wolffia columbiana"]) >0 ) {
    
    # combine the results 
    dataWA_lakes_FP_ever <- rbind(dataWA_lakes_FP_ever, temp)
    
  }
}
rm(temp,i) # clean up

# check out the top of the data frame 
dataWA_lakes_FP_ever[1:20,]

# write this to csv for exploring 
write.csv(dataWA_lakes_FP_ever,"dataWA_lakes_FP_ever.csv")

# number of lakes that have FP @ some point in time (not necessarily most recent survey)
length(unique(dataWA_lakes_FP_ever$site))

# combine "Azolla" and "Azolla cristata" 
dataWA_lakes_FP_ever["Azolla"] <- dataWA_lakes_FP_ever["Azolla"] + dataWA_lakes_FP_ever["Azolla cristata"]
dataWA_lakes_FP_ever["Azolla cristata"] <- NULL 

# combine "Wolffia" and "Wolffia columbiana" 
dataWA_lakes_FP_ever["Wolffia"] <- dataWA_lakes_FP_ever["Wolffia"] + dataWA_lakes_FP_ever["Wolffia columbiana"]
dataWA_lakes_FP_ever["Wolffia columbiana"] <- NULL 

# reshape for plotting 
dataWA_lakes_FP_ever_reshape <- melt(dataWA_lakes_FP_ever, id.vars=c("site","year"))
colnames(dataWA_lakes_FP_ever_reshape)[3] <- "species"
colnames(dataWA_lakes_FP_ever_reshape)[4] <- "presence"

# plot gain / loss of species 
for (i in unique(dataWA_lakes_FP_ever_reshape$species)) {
  
  temp_data <- subset(dataWA_lakes_FP_ever_reshape, dataWA_lakes_FP_ever_reshape$species == i)
  
  temp_plot <- ggplot(temp_data, aes(x=year,y=presence,group=site,colour=site))
  temp_plot <- temp_plot + geom_point()
  temp_plot <- temp_plot + geom_line(alpha=0.4)
  temp_plot <- temp_plot + theme_classic()
  temp_plot <- temp_plot + theme(legend.position="none")
  temp_plot <- temp_plot + ggtitle(i)
  temp_plot
  
  ggsave(file=paste("colonize_plot_",i,".jpg",sep=""),temp_plot,height=8,width=11)
}
  
###################### 
# one row per site   #
# 1 survey/lake      # 
# most recent survey #
######################
# for each site pick the most recent survey
# make a new data frame 
dataWA_onepersite <- NULL

for (i in unique(dataWA_spmatrix$site)){
  # get all records (surveys) for a single site 
  temp <- subset(dataWA_spmatrix, dataWA_spmatrix$site==i) 
  
  # pick the most recent survey 
  temp <- subset(temp, temp$year == max(temp$year))
  
  # combine the results 
  dataWA_onepersite <- rbind(dataWA_onepersite, temp)
}

dataWA_onepersite

#######################################
# Re-import Latitude/Longitude values #
#######################################
# I want "dataWA_onepersite" to have Lat/Lon values 

# build a temporary data frame 
temp_data <- cbind(as.character(dataWA$Location_ID),dataWA$Calculated_Latitude_Decimal_Degrees_NAD83HARN,dataWA$Calculated_Longitude_Decimal_Degrees_NAD83HARN)
temp_data <- unique(temp_data) # now just one per lake 
temp_data <- as.data.frame(temp_data) # now just one per lake 
colnames(temp_data) <- c("site","latitude","longitude")
temp_data

# merge  based on site ID 
dataWA_onepersite <- merge(dataWA_onepersite,temp_data,by.x="site",by.y="site")

# clean up 
rm(temp_data)

#####################################
# List of species & their freq.     #
# reshape the site x species matrix #
#####################################
# species and the # of water bodies they are found in 

temp<-colSums(dataWA_onepersite[,2:253]) # add up to get frequency of occurence
temp2<-colnames(dataWA_onepersite[,2:253]) # save names of species 
dataWA_SPfreq <-cbind(temp2,as.data.frame(temp)) # combine names of species and frequency of occurence 
rm(temp,temp2) # remove unwanted objects 
colnames(dataWA_SPfreq)<-c("species","frequency") # re-name the columns
row.names(dataWA_SPfreq)<-1:nrow(dataWA_SPfreq) # re-name the rows
dataWA_SPfreq <- dataWA_SPfreq[order(dataWA_SPfreq[,2],decreasing=TRUE),] # re-order (decreasing frequency)
dataWA_SPfreq <- within(dataWA_SPfreq,species <- factor(species,levels=species)) # re-order so it will plot w/ decreasing bar height
dataWA_SPfreq
write.csv(dataWA_SPfreq,"dataWA_SPfreq.csv",row.names=FALSE)

##########################################
# Add labels and combine some FP species #
##########################################
# open up dataWA_SPfreq.csv
# add FP labels 
# remove species that are 0's (they were not found in the most recent surveys)
# combine Azolla 
# combine Wolffia 

# read it back in 
dataWA_SPfreq <- read.csv("dataWA_SPfreq.csv")

###################
# Just FP species #
# one per site    # 
# most recent     #
###################
dataWA_FPonly <- cbind(dataWA_onepersite["Azolla"],dataWA_onepersite["Lemna minor"],
                       dataWA_onepersite["Lemna"],dataWA_onepersite["Lemna trisulca"],
                       dataWA_onepersite["Spirodela polyrrhiza"],dataWA_onepersite["Wolffia"])

WA_FP_richness <- rowSums(dataWA_FPonly)

dataWA_FPonly$FP_richness <- rowSums(dataWA_FPonly)

table(dataWA_FPonly$FP_richness)

dataWA_FPonly <- cbind(dataWA_FPonly,dataWA_onepersite["year"],dataWA_onepersite["site"],dataWA_onepersite["latitude"],dataWA_onepersite["longitude"])

# find out about the one water body that has FP richness = 5 
subset(dataWA_FPonly, dataWA_FPonly$FP_richness==5)

# convert latitude and longitude to numeric values that can be plotted 
dataWA_FPonly$latitude <- as.numeric(levels(dataWA_FPonly$latitude))[dataWA_FPonly$latitude]  
dataWA_FPonly$longitude <- as.numeric(levels(dataWA_FPonly$longitude))[dataWA_FPonly$longitude]

#################### 
# FP richness plot #
# one per site     # 
# most recent      #
####################
WA_FP_richness_matrix <- matrix(c(0,1,2,3,4,5,436,45,17,3,0,1),nrow=6,ncol=2,byrow=F)
WA_FP_richness_matrix <- as.data.frame(WA_FP_richness_matrix)
colnames(WA_FP_richness_matrix)[1] <- "richness"
colnames(WA_FP_richness_matrix)[2] <- "frequency"
WA_FP_richness_matrix

hist_WA_FP_rich <- ggplot(WA_FP_richness_matrix, aes(x=richness,y=frequency)) + geom_bar(stat="identity",fill = I("grey50"))
hist_WA_FP_rich <- hist_WA_FP_rich + xlab("Floating plant richness")
hist_WA_FP_rich <- hist_WA_FP_rich + ylab("Frequency")
hist_WA_FP_rich <- hist_WA_FP_rich + scale_x_continuous(breaks=c(0,1,2,3,4,5))
hist_WA_FP_rich <- hist_WA_FP_rich + theme_classic(base_size=18)
hist_WA_FP_rich <- hist_WA_FP_rich + ggtitle("Washington, USA - n=502 lakes")
hist_WA_FP_rich
ggsave("hist_WA_FP_rich.jpg",hist_WA_FP_rich,height=8,width=8)

###########################################
# Return your working directory @ the end #
###########################################
setwd("C:/Users/Mike/Desktop/Dropbox/absences")