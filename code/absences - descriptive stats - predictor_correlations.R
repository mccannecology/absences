load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - data imported.RData")

library(ggplot2)

dataPREDICTORS <- data[,c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                          "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                          "dist_waterfowl","nearest_LM","nearest_SP","nearest_W","latitude","longitude",
                          "boatlaunch","nonFP_species_richness")]

###################
# Bivariate plots # 
###################

predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_LM","nearest_SP","nearest_W","latitude","longitude",
                 "boatlaunch","nonFP_species_richness") 

# non-transformed
for(i in unique(predict_var)){
  for(j in unique(predict_var)){
  
    PLOT <- ggplot(data, aes_string(x=i, y=j))
    PLOT <- PLOT + geom_point()
    PLOT <- PLOT + xlab(i) + ylab(j)
    PLOT <- PLOT + stat_smooth(method='lm')
    PLOT <- PLOT + theme_classic(base_size=18)
    PLOT
    
    ggsave(paste("corrl_",i,"_vs_",j,"_data.jpg",sep=""),PLOT,height=4,width=4)
    rm(PLOT)
    
  }
}

# transformed (scaled 0-1 and skewness reduced)
for(i in unique(predict_var)){
  for(j in unique(predict_var)){
    
    PLOT <- ggplot(dataENV_trans, aes_string(x=i, y=j))
    PLOT <- PLOT + geom_point()
    PLOT <- PLOT + xlab(i) + ylab(j)
    PLOT <- PLOT + stat_smooth(method='lm')
    PLOT <- PLOT + theme_classic(base_size=18)
    PLOT
    
    ggsave(paste("corrl_",i,"_vs_",j,"_dataENV_trans.jpg",sep=""),PLOT,height=4,width=4)
    rm(PLOT)
    
  }
}

# transformed (scaled 0-1 only)
for(i in unique(predict_var)){
  for(j in unique(predict_var)){
    
    PLOT <- ggplot(dataENV_scaled, aes_string(x=i, y=j))
    PLOT <- PLOT + geom_point()
    PLOT <- PLOT + xlab(i) + ylab(j)
    PLOT <- PLOT + stat_smooth(method='lm')
    PLOT <- PLOT + theme_classic(base_size=18)
    PLOT
    
    ggsave(paste("corrl_",i,"_vs_",j,"_dataENV_scaled.jpg",sep=""),PLOT,height=4,width=4)
    rm(PLOT)
    
  }
}


################################### 
# Heterogenous correlation matrix # dataENV_trans
################################### 
library(polycor)

# set-up a temporary data frame (you need to transform one variable)
dataENV_trans_cor <- dataENV_trans
dataENV_trans_cor$boatlaunch <- as.factor(temp$boatlaunch)

# use the hetergeneous correlation matrix 
corrl_dataENV_trans <- hetcor(dataENV_trans_cor)

# clean up 
rm(dataENV_trans_cor)

# compare it to the base cor() function
cor(dataENV_trans)

################################### 
# Heterogenous correlation matrix # dataENV_scaled
################################### 
library(polycor)

# set-up a temporary data frame (you need to transform one variable)
dataENV_scaled_cor <- dataENV_scaled

# use the hetergeneous correlation matrix 
corrl_dataENV_scaled <- hetcor(dataENV_scaled_cor)

# clean up 
rm(dataENV_scaled_cor)

# compare it to the base cor() function
cor(dataENV_scaled)

#################################################
# Plotting the heterogeneous correlation matrix # dataENV_trans
################################################# 
# http://stackoverflow.com/questions/12196756/significance-level-added-to-matrix-correlation-heatmap-using-ggplot2

library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)

# just return the correlation matrix 
corrl_dataENV_trans_cors <- corrl_dataENV_trans$correlations

# rename the rows and columns to match the manuscript 
colnames(corrl_dataENV_trans_cors) <- c("size","shoreline","depth","totalP","pH","cond","alk","secchi","lakes1km","lakes10km","waterfowl",
                                        "distLM","distSP","distW","dist_any_FP","latitude","longitude","boatlaunch","nonFP")
rownames(corrl_dataENV_trans_cors) <- c("size","shoreline","depth","totalP","pH","cond","alk","secchi","lakes1km","lakes10km","waterfowl",
                                        "distLM","distSP","distW","dist_any_FP","latitude","longitude","boatlaunch","nonFP")

# remove unwanted variables (dist_any_FP) to match the manuscript 
corrl_dataENV_trans_cors <- corrl_dataENV_trans_cors[!rownames(corrl_dataENV_trans_cors) %in% c("dist_any_FP"), ]
corrl_dataENV_trans_cors <- corrl_dataENV_trans_cors[,!colnames(corrl_dataENV_trans_cors) %in% c("dist_any_FP")]

# save as a data frame 
corrl_dataENV_trans_cors <- as.data.frame(corrl_dataENV_trans_cors)

# create a column named "row"
corrl_dataENV_trans_cors <- data.frame(row=rownames(corrl_dataENV_trans_cors), corrl_dataENV_trans_cors)

# remove row names 
rownames(corrl_dataENV_trans_cors) <- NULL

# melt the data frame 
corrl_dataENV_trans_cors <- melt(corrl_dataENV_trans_cors)

# make cuts in the correlations
corrl_dataENV_trans_cors$value <- cut(corrl_dataENV_trans_cors$value,
                                        breaks=c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0),
                                        include.lowest=TRUE,
                                        label=c("(-0.8,-1)","(-0.6,-0.8)","(-0.4,-0.6)","(-0.2,-0.4)","(0,-0.2)","(0,0.2)",
                                                "(0.2,0.4)","(0.4,0.6)","(0.6,0.8)","(0.8,1.0)")) 

# reorder the "row" column which would be used as the x axis in the plot after converting it to a factor and ordered now
corrl_dataENV_trans_cors$row <- factor(corrl_dataENV_trans_cors$row, 
                                         levels=rev(unique(as.character(corrl_dataENV_trans_cors$variable)))) 

# useful to get rid of grids of plot taken from https://gist.github.com/1035189/ac763cb4480c7b522483fa90ed0865d66593737c
po.nopanel <- list(opts(panel.background=theme_blank(),panel.grid.minor=theme_blank(),panel.grid.major=theme_blank())) 

# plot it 
corr_plot <- ggplot(corrl_dataENV_trans_cors, aes(row, variable)) 
corr_plot <- corr_plot + geom_tile(aes(fill=value),colour="black") 
corr_plot <- corr_plot + scale_fill_brewer(palette = "RdYlBu", name="Correlation")  # here comes the RColorBrewer package
corr_plot <- corr_plot + theme_grey(base_size=18)
corr_plot <- corr_plot + ylab("")
corr_plot <- corr_plot + theme(axis.text.x=element_text(angle=315,hjust=0))
corr_plot <- corr_plot + po.nopanel
corr_plot

ggsave("corrl_matrix_heatmap_dataENV_trans.jpg",corr_plot,height=8, width=11)

#################################################
# Plotting the heterogeneous correlation matrix # dataENV_scaled
################################################# 
# http://stackoverflow.com/questions/12196756/significance-level-added-to-matrix-correlation-heatmap-using-ggplot2

library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)

# just return the correlation matrix 
corrl_dataENV_scaled_cors <- corrl_dataENV_scaled$correlations

# rename the rows and columns to match the manuscript 
colnames(corrl_dataENV_scaled_cors) <- c("size","shoreline","depth","totalP","pH","cond","alk","secchi","lakes1km","lakes10km","waterfowl",
                                        "distLM","distSP","distW","dist_any_FP","latitude","longitude","boatlaunch","nonFP")
rownames(corrl_dataENV_scaled_cors) <- c("size","shoreline","depth","totalP","pH","cond","alk","secchi","lakes1km","lakes10km","waterfowl",
                                        "distLM","distSP","distW","dist_any_FP","latitude","longitude","boatlaunch","nonFP")

# remove unwanted variables (dist_any_FP) to match the manuscript 
corrl_dataENV_scaled_cors <- corrl_dataENV_scaled_cors[!rownames(corrl_dataENV_scaled_cors) %in% c("dist_any_FP"), ]
corrl_dataENV_scaled_cors <- corrl_dataENV_scaled_cors[,!colnames(corrl_dataENV_scaled_cors) %in% c("dist_any_FP")]

# save as a data frame 
corrl_dataENV_scaled_cors <- as.data.frame(corrl_dataENV_scaled_cors)

# create a column named "row"
corrl_dataENV_scaled_cors <- data.frame(row=rownames(corrl_dataENV_scaled_cors), corrl_dataENV_scaled_cors)

# remove row names 
rownames(corrl_dataENV_scaled_cors) <- NULL

# melt the data frame 
corrl_dataENV_scaled_cors <- melt(corrl_dataENV_scaled_cors)

# make cuts in the correlations
corrl_dataENV_scaled_cors$value <- cut(corrl_dataENV_scaled_cors$value,
                                      breaks=c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0),
                                      include.lowest=TRUE,
                                      label=c("(-0.8,-1)","(-0.6,-0.8)","(-0.4,-0.6)","(-0.2,-0.4)","(0,-0.2)","(0,0.2)",
                                              "(0.2,0.4)","(0.4,0.6)","(0.6,0.8)","(0.8,1.0)")) 

# reorder the "row" column which would be used as the x axis in the plot after converting it to a factor and ordered now
corrl_dataENV_scaled_cors$row <- factor(corrl_dataENV_scaled_cors$row, 
                                       levels=rev(unique(as.character(corrl_dataENV_scaled_cors$variable)))) 

# useful to get rid of grids of plot taken from https://gist.github.com/1035189/ac763cb4480c7b522483fa90ed0865d66593737c
po.nopanel <- list(opts(panel.background=theme_blank(),panel.grid.minor=theme_blank(),panel.grid.major=theme_blank())) 

# plot it 
corr_plot <- ggplot(corrl_dataENV_scaled_cors, aes(row, variable))
corr_plot <- corr_plot + geom_tile(aes(fill=value),colour="black") 
corr_plot <- corr_plot + scale_fill_brewer(palette = "RdYlBu", name="Correlation")  # here comes the RColorBrewer package
corr_plot <- corr_plot + theme_grey(base_size=18)
corr_plot <- corr_plot + ylab("")
corr_plot <- corr_plot + theme(axis.text.x=element_text(angle=315,hjust=0))
corr_plot <- corr_plot + po.nopanel
corr_plot

ggsave("corrl_matrix_heatmap_dataENV_scaled.jpg",corr_plot,height=8, width=11)


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


############## 
# Save stuff #
############## 