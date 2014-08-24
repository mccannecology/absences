####################################
# Transforming predictor variables #
# Goals: 0 skewness, range 0-1     #
####################################

head(dataENV)

# goal: make a new data frame "dataENV_trans" 
# with transformed variables that make 0 skewness
# and variables range from 0 to 1 

# add latitude and longitude to the data frame (make a new data frame)
dataPREDICTORS <- cbind(dataENV,dataSPACE[,1:2])
head(dataPREDICTORS)

####################################
# check skewness for each variable # 
# with different transformations   # 
####################################
# load packages 
library(moments)

# untransformed
# loop through each variable 
for (i in 1:ncol(dataPREDICTORS)){
  temp <- skewness(dataPREDICTORS[,i])
  variable <- colnames(dataPREDICTORS)[i]
  print(c(variable,temp))
}

# log x + 1 transformed 
# loop through each variable 
for (i in 1:ncol(dataPREDICTORS)){
  temp <- skewness(log(dataPREDICTORS[,i]+1))
  variable <- colnames(dataPREDICTORS)[i]
  print(c(variable,temp))
}

# log x  transformed 
# loop through each variable 
for (i in 1:ncol(dataPREDICTORS)){
  temp <- skewness(log(dataPREDICTORS[,i]))
  variable <- colnames(dataPREDICTORS)[i]
  print(c(variable,temp))
}

# power transformed 
# power transformation will not work on variables <0 (waterbodies_1km, boatlaunch, dist_waterfowl)
library(car)
for (i in 1:ncol(dataPREDICTORS)){
  variable <- colnames(dataPREDICTORS)[i]
    
  if (variable == "waterbodies_1km" | variable == "boatlaunch" | variable == "dist_waterfowl"){
    print(paste("*** Skipped ",variable," values <=0",sep=""))
  }
  else {
    power <- powerTransform(dataPREDICTORS[,i])$lambda
    temp <- skewness(((dataPREDICTORS[,i])^power - 1) / power)
    #print(c(variable,temp)) 
    print(power)
  }
}

####################################
# Creat the transformed data frame #
####################################
# add each variable with its best transformation (skewness closest to 0)
dataENV_trans <- cbind(log(dataPREDICTORS$surfacearea_ha + 1),
                       ((dataPREDICTORS$shoreline_development^-0.8704562 - 1) / -0.8704562),
                       ((dataPREDICTORS$depth_max_m^-0.01880383 - 1) / -0.01880383),
                       ((dataPREDICTORS$TOTP_avg^-0.01574937 - 1) / -0.01574937),
                       ((dataPREDICTORS$PH_avg^-0.6989874 - 1) / -0.6989874),
                       ((dataPREDICTORS$COND_avg^0.1692474 - 1) / 0.1692474),
                       ((dataPREDICTORS$ALK_avg^0.1444776 - 1) / 0.1444776),
                       ((dataPREDICTORS$secchi_avg^0.1559967 - 1) / 0.1559967),
                       log(dataPREDICTORS$waterbodies_1km + 1),
                       ((dataPREDICTORS$waterbodies_5km^0.1439263 - 1) / 0.1439263),
                       ((dataPREDICTORS$waterbodies_10km^0.2475298 - 1) / 0.2475298),
                       log(dataPREDICTORS$dist_waterfowl + 1),
                       ((dataPREDICTORS$nearest_LM^0.3773737 - 1) / 0.3773737),
                       ((dataPREDICTORS$nearest_SP^0.6108591 - 1) / 0.6108591),
                       ((dataPREDICTORS$nearest_W^0.5045787 - 1) / 0.5045787),
                       ((dataPREDICTORS$nearest_LMSPW^0.4613516 - 1) / 0.4613516),
                       ((dataPREDICTORS$latitude^-7.304205 - 1) / -7.304205),
                       ((dataPREDICTORS$longitude^4.331463 - 1) / 4.331463), 
                       dataPREDICTORS$boatlaunch
)


# Range the variables from 0-1
# make sure boatlaunch is the last column
# I don't want to "range" (transform to 0-1) this variable

for (i in 1:(ncol(dataENV_trans)-1)){
  #print(i)
  dataENV_trans[,i] <- rescale(dataENV_trans[,i])
}

colnames(dataENV_trans) <- c("surfacearea_ha",
                             "shoreline_development",
                             "depth_max_m",
                             "TOTP_avg",
                             "PH_avg",
                             "COND_avg",
                             "ALK_avg",
                             "secchi_avg",
                             "waterbodies_1km",
                             "waterbodies_5km",
                             "waterbodies_10km",
                             "dist_waterfowl",
                             "nearest_LM",
                             "nearest_SP",
                             "nearest_W",
                             "nearest_LMSPW",
                             "latitude",
                             "longitude",
                             "boatlaunch"
)

dataENV_trans <- as.data.frame(dataENV_trans)

######################################
# Remove highly correlated variables #
######################################
dataENV_trans["ALK_avg"] <- NULL 
dataENV_trans["waterbodies_5km"] <- NULL 
dataENV_trans["nearest_LMSPW"] <- NULL 
dataENV_trans["depth_max_m"] <- NULL 
  
colnames(dataENV_trans)

#############################
# clean up when you're done #
#############################
rm(temp,i,power,variable,dataPREDICTORS)
  
  
  