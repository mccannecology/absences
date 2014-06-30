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
data<-subset(data, !(waterbody == "Mackenzie Reservoir  2  small" & year == "comb")) 
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

write.csv(data,"data-cleanedup.csv",row.names=FALSE)

########################################
# Create species, environment matrices #
########################################

dataSPECIES <- 

dataFP 

dataNONFP

dataENV

 


