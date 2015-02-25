load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - data imported.RData")

# Make a new data frame with all of the predictors 
dataPREDICTORS <- dataENV 
dataPREDICTORS$latitude <- data$latitude 
dataPREDICTORS$longitude <- data$longitude 
dataPREDICTORS$nearest_any_FP <- data$nearest_any_FP 


###################
# Bivariate plots # 
###################

predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_LM","nearest_SP","nearest_W","latitude","longitude",
                 "boatlaunch","nonFP_species_richness") 

for(i in unique(predict_var)){
  for(j in unique(predict_var)){
  
    PLOT <- ggplot(dataPREDICTORS, aes_string(x=i, y=j))
    PLOT <- PLOT + geom_point()
    PLOT <- PLOT + xlab(i) + ylab(j)
    PLOT <- PLOT + stat_smooth()
    PLOT <- PLOT + theme_classic(base_size=18)
    PLOT
    
    ggsave(paste("corrl_",i,"_vs_",j,".jpg",sep=""),PLOT,height=4,width=4)
    rm(PLOT)
    
  }
}

######################### 
# Rank the corerlations # dataPREDICTORS
#########################
corrl_rank_dataPREDICTORS <- cor(as.matrix(dataPREDICTORS[,sapply(dataPREDICTORS,is.numeric)]),use="pairwise.complete.obs")
corrl_rank_dataPREDICTORS[lower.tri(corrl_rank_dataPREDICTORS,diag=TRUE)] <- NA       # Prepare to drop duplicates and meaningless information
corrl_rank_dataPREDICTORS <- as.data.frame(as.table(corrl_rank_dataPREDICTORS))       # Turn into a 3-column table
corrl_rank_dataPREDICTORS <- na.omit(corrl_rank_dataPREDICTORS)                       # Get rid of the junk we flagged above
corrl_rank_dataPREDICTORS <- corrl_rank_dataPREDICTORS[order(-abs(corrl_rank_dataPREDICTORS$Freq)),] # sort the data frame by absolute value of the correlation  
colnames(corrl_rank_dataPREDICTORS)[3] <- "corrl"
corrl_rank_dataPREDICTORS

write.csv(corrl_rank_dataPREDICTORS,"corrl_rank_dataPREDICTORS.csv",row.names=FALSE)

######################### 
# Rank the corerlations # dataENV_trans
#########################
corrl_rank_dataENV_trans <- cor(as.matrix(dataENV_trans[,sapply(dataENV_trans,is.numeric)]),use="pairwise.complete.obs")
corrl_rank_dataENV_trans[lower.tri(corrl_rank_dataENV_trans,diag=TRUE)] <- NA       # Prepare to drop duplicates and meaningless information
corrl_rank_dataENV_trans <- as.data.frame(as.table(corrl_rank_dataENV_trans))       # Turn into a 3-column table
corrl_rank_dataENV_trans <- na.omit(corrl_rank_dataENV_trans)                       # Get rid of the junk we flagged above
corrl_rank_dataENV_trans <- corrl_rank_dataENV_trans[order(-abs(corrl_rank_dataENV_trans$Freq)),] # sort the data frame by absolute value of the correlation  
colnames(corrl_rank_dataENV_trans)[3] <- "corrl"
corrl_rank_dataENV_trans

write.csv(corrl_rank_dataENV_trans,"corrl_rank_dataENV_trans.csv",row.names=FALSE)
