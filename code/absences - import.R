##############################################
# ANALYSIS OF CT & LI DUCKWEED SURVEY DATA   #
#                                            #
# Presence / Absence of floating plants      # 
# Importing data sets                        #
# Created: 06/24/2014 by MJ McCann           #
##############################################

###############################################################################################
# NOTE: SAVING EXCEL FILES FOR IMPORTING 
# save main database ("survey analysis all xx-xx-20xx.xls") as "allsurveys+combined.csv" 
# open up allsurveys+combined.csv
# CTRL + F and replace all "-" with ""
###############################################################################################

data <- read.csv("allsurveys+combined.csv") # import data matrix 

# Remove 2004 CAES surveys (did not record presence/absence of FP)
data <- subset(data, year != "2004")

# Remove my surveys (still includes water bodies surveyed by me & CAES )
data <- subset(data, data_source != "MJM")

# Remove unwanted variables 
drops <- c("dateNUTR","off_season","surfaceaarea_sqm","perimeter_m","depth_avg_m","forested_shoreline_prop",
           "inflow","outflow","regime","azolla_cover_prop","lminor_cover_prop","lvaldiviana_cover_prop",
           "ltrisulca_cover_prop","riccia_cover_prop","spolyrhiza_cover_prop","wolffia_cover_prop",
           "wbrasiliensis_cover_prop","wborealis_cover_prop","PO4_n","PO4_avg","PO4_SE","TOTN_n","TOTN_avg","TOTN_SE",
           "NO3_n","NO3_avg","NO3_SE","NH4_n","NH4_avg","NH4_SE","totNtotP_n","totNtotP_avg","totNtotP_SE","notes")
data <- data[,!(names(data) %in% drops)]
rm(drops)

# check out if the variables look correct (e.g., factors vs. numeric)
str(data)

######################################################### 
# Water bodies w/ 2 surveys & one was in 2004           #
# Remove the combined survey (2004 already removed)     #
# Unless missing data can be estimated from 2004 survey #
#########################################################
# Number of water bodies with multiple surveys 
# nrow(subset(data, year == "comb")) # 43 

# Anderson Pond
data<-subset(data, !(waterbody == "Anderson Pond" & year == "comb"))
# Beseck Lake
data<-subset(data, !(waterbody == "Beseck Lake" & year == "comb"))
# Canoe Brook Lake  
data<-subset(data, !(waterbody == "Canoe Brook Lake" & year == "comb"))
# North Farms Reservoir
data<-subset(data, !(waterbody == "North Farms Reservoir" & year == "comb"))
# Pinewood Lake 
data<-subset(data, !(waterbody == "Pinewood Lake" & year == "comb"))
# Quonnipaug Lake
data<-subset(data, !(waterbody == "Quonnipaug Lake" & year == "comb"))
# Silver Lake
data<-subset(data, !(waterbody == "Silver Lake" & year == "comb"))
# Wintergreen Lake
data<-subset(data, !(waterbody == "Wintergreen Lake" & year == "comb"))

# Granniss Lake - Esimate missing values from 2004? 
# I won't for now (remove the combined survey)
data<-subset(data, !(waterbody == "Graniss Lake" & year == "comb"))

####################################
# Water bodies CAES & MJM surveys  #
# Remove my survey                 #
####################################
# North Farms Reservoir * Also appears on the previous list - already removed 

# Birch Pond 
data<-subset(data, !(waterbody == "Birch Pond" & year == "comb"))
# Branford Supply Pond (East)
data<-subset(data, !(waterbody == "Branford Supply Pond (East)" & year == "comb"))
# Cedar Pond 
data<-subset(data, !(waterbody == "Cedar Pond" & year == "comb"))
# Dayton Pond
data<-subset(data, !(waterbody == "Dayton Pond" & year == "comb")) 
# Lucky Pond (Galko Farm Pond)
data<-subset(data, !(waterbody == "Lucky Pond (Galko Farm Pond)" & year == "comb"))
# Mackenzie Reservoir - 2 - small
data<-subset(data, !(waterbody == "Mackenzie Reservoir - 2 - small" & year == "comb")) 
# Mill Pond Park
data<-subset(data, !(waterbody == "Mill Pond Park" & year == "comb"))
# Mills Pond, Lower
data<-subset(data, !(waterbody == "Mills Pond, Lower" & year == "comb"))
# Mills Pond, Upper
data<-subset(data, !(waterbody == "Mills Pond, Upper" & year == "comb"))
# Tilleys Pond
data<-subset(data, !(waterbody == "Tilleys Pond" & year == "comb"))
# Town Mill Pond
data<-subset(data, !(waterbody == "Town Mill Pond" & year == "comb")) 
# Young's Pond 
data<-subset(data, !(waterbody == "Young's Pond" & year == "comb"))

# Mackenzie Reservoir - 1 - large - does not contain an CAES chemistry data - can estimate with my survey
# I will just remove this waterbody for now 
data<-subset(data, !(waterbody == "Mackenzie Reservoir  1  large"))

###############################################
# 21 water bodies with 2 CAES surveys         #
# (not in 2004 and without missing variables) #
###############################################
unique(data$waterbody[duplicated(data$waterbody)])

# use the combined values for now 

# Amos Lake            
data<-subset(data, !(waterbody == "Amos Lake" & year != "comb"))
# Bolton Lake, Lower      
data<-subset(data, !(waterbody == "Bolton Lake, Lower" & year != "comb"))
# Bolton Lake, Middle     
data<-subset(data, !(waterbody == "Bolton Lake, Middle" & year != "comb"))
# Cedar Lake             
data<-subset(data, !(waterbody == "Cedar Lake" & year != "comb"))
# Crystal Lake (1)        
data<-subset(data, !(waterbody == "Crystal Lake (1)" & year != "comb"))
# Fence Rock Lake       
data<-subset(data, !(waterbody == "Fence Rock Lake" & year != "comb"))
# Hayward Lake           
data<-subset(data, !(waterbody == "Hayward Lake" & year != "comb"))
# Laurel Lake           
data<-subset(data, !(waterbody == "Laurel Lake" & year != "comb"))
# Long Meadow Pond     
data<-subset(data, !(waterbody == "Long Meadow Pond" & year != "comb"))
# Lower Moodus Reservoir 
data<-subset(data, !(waterbody == "Lower Moodus Reservoir" & year != "comb"))
# Mamanasco Lake         
data<-subset(data, !(waterbody == "Mamanasco Lake" & year != "comb"))
# Moosup Pond          
data<-subset(data, !(waterbody == "Moosup Pond" & year != "comb"))
# Pattaganset Lake     
data<-subset(data, !(waterbody == "Pattaganset Lake" & year != "comb"))
# Rolling Ridge Pond     
data<-subset(data, !(waterbody == "Rolling Ridge Pond" & year != "comb"))
# Staffordville Reservoir
data<-subset(data, !(waterbody == "Staffordville Reservoir" & year != "comb"))
# Taunton Lake          
data<-subset(data, !(waterbody == "Taunton Lake" & year != "comb"))
# Twin Lake, North     
data<-subset(data, !(waterbody == "Twin Lake, North" & year != "comb"))
# Tyler Lake            
data<-subset(data, !(waterbody == "Tyler Lake" & year != "comb"))
# West Hill Pond        
data<-subset(data, !(waterbody == "West Hill Pond" & year != "comb"))
# West Lake              
data<-subset(data, !(waterbody == "West Lake" & year != "comb"))
# West Side Pond    
data<-subset(data, !(waterbody == "West Side Pond" & year != "comb"))

#########################################
# Remove extra variables
# i.e., sample size # 
# Remove water bodies w/ missing values #
#########################################
# remove sample size (n) and SEs 
drops <- c("TOTP_n","TOTP_SE","PH_n","PH_SE","COND_n","COND_SE","DO_n","DO_SE","TEMP_n","TEMP_SE","ALK_n","ALK_SE","secchi_n","secchi_SE")
data <- data[,!(names(data) %in% drops)]

# remove DO and TEMP (they will be highly-dependent on survey date)
data <- data[,-34]
data <- data[,-34]

# Remove rows with missing varaibles - 8 water bodies
# Beeslick, Eagleville, Granniss, Quassapang, Slopers, Talmadge, Williamson, Wyler
data <- data[complete.cases(data),]

# Remove any columns where all values are 0
drops <- c("potamogeton_vase","potamogeton_frie","nymphoides_pelt","nymphaea_odor_odor",
           "ludwigia_lac","ludwigia_dub","elatine_triandra","ellocharis_parv","callitriche_verna")
data <- data[,!(names(data) %in% drops)]

# remove 4H camp - no plants were present 
data <- data[-1,]

rm(drops)

# Write this to a .csv so I can see what I did 
write.csv(data,"data-cleanedup.csv",row.names=FALSE)

####################################
# Add variables extracted from GIS # 
####################################
data <- read.csv("data-with_GIS_variables.csv")

########################################
# Create species, environment matrices #
########################################
colnames(data)

# matrix of latitude and longitudes 
dataSPACE <- data[,5:6]
write.csv(dataSPACE,"dataSPACE.csv",row.names=FALSE)

# matrix of environmental variables 
dataENV <- cbind(data[,10:12], data[,31:35],data[,153:157])
write.csv(dataENV,"dataENV.csv",row.names=FALSE)

# matrix of floating plant species presence/absence
dataFP <- data[,18:27]
dataFP <- dataFP[,-5] # remove Lemna valdiviana & Riccia sp. (only occur in my surveys)
dataFP <- dataFP[,-5]
dataFP$wolffia <- dataFP$wolffia + dataFP$wolffia_borealis + dataFP$wolffia_brasiliensis # combine the wolffias 
dataFP <- dataFP[,-7]
dataFP <- dataFP[,-7]
colnames(dataFP)
write.csv(dataFP,"dataFP.csv",row.names=FALSE)

# matrix of non-floating plant species presence/absence 
dataNONFP <- data[,36:152]
write.csv(dataNONFP,"dataNONFP.csv",row.names=FALSE)

# matrix of both floating and non-floating plant species presence/absence 
dataSPECIES <- cbind(dataFP,dataNONFP)
write.csv(dataSPECIES,"dataSPECIES.csv",row.names=FALSE)

# matrix of species and the # of water bodies they are found in 
temp<-colSums(dataSPECIES) # add up to get frequency of occurence
temp2<-colnames(dataSPECIES) # save names of species 
dataSPECIES_freq <-cbind(temp2,as.data.frame(temp)) # combine names of species and frequency of occurence 
rm(temp) # remove unwanted objects 
rm(temp2) # remove unwanted objects
colnames(dataSPECIES_freq)<-c("species","frequency") # re-name the columns
row.names(dataSPECIES_freq)<-1:nrow(dataSPECIES_freq) # re-name the rows
dataSPECIES_freq <- dataSPECIES_freq[order(dataSPECIES_freq[,2],decreasing=TRUE),] # re-order (decreasing frequency)
dataSPECIES_freq <- within(dataSPECIES_freq,species <- factor(species,levels=species)) # re-order so it will plot w/ decreasing bar height
dataSPECIES_freq
write.csv(dataSPECIES_freq,"dataSPECIES_freq.csv",row.names=FALSE)

# add wolffia_sp. to data 
data$wolffia_sp <- data$wolffia + data$wolffia_borealis + data$wolffia_brasiliensis # combine the wolffias 
colnames(data)


