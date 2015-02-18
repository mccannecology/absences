############### 
# LEMNA MINOR #
###############
predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_LM","latitude","longitude","boatlaunch","nonFP_species_richness") 

for(i in unique(predict_var)){
  
  PLOT <- ggplot(data, aes_string(x="lemna_minor", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("L. minor presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("lemna_minor_vs_",i,".jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
  PLOT <- ggplot(temp_data_LM, aes_string(x="LM", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("L. minor presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("lemna_minor_vs_",i,"_trans.jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
}

############### 
# SPIRODELA   #
###############
predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_SP","latitude","longitude","boatlaunch","nonFP_species_richness") 

for(i in unique(predict_var)){
  
  PLOT <- ggplot(data, aes_string(x="spirodela_polyrhiza", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("S. polyrhiza presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("spirodela_polyrhiza_vs_",i,".jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
  PLOT <- ggplot(temp_data_SP, aes_string(x="SP", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("S. polyrhiza presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("spirodela_polyrhiza_vs_",i,"_trans.jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
}


############### 
# WOLFFIA   #
###############
predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_W","latitude","longitude","boatlaunch","nonFP_species_richness") 

for(i in unique(predict_var)){
  
  PLOT <- ggplot(data, aes_string(x="wolffia_sp", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("Wolffia sp. presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("wolffia_vs_",i,".jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
  PLOT <- ggplot(temp_data_W, aes_string(x="W", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("Wolffia sp. presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("wolffia_vs_",i,"_trans.jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
}



############### 
# FP presence #
###############
predict_var <- c("surfacearea_ha","shoreline_development","depth_max_m","TOTP_avg",
                 "PH_avg","COND_avg","ALK_avg","secchi_avg","waterbodies_1km","waterbodies_10km",
                 "dist_waterfowl","nearest_LM","nearest_SP","nearest_W","latitude","longitude",
                 "boatlaunch","nonFP_species_richness") 

for(i in unique(predict_var)){
  
  PLOT <- ggplot(data, aes_string(x="FP_presence", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("Floating plant presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("FPpres_vs_",i,".jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
  PLOT <- ggplot(temp_data_FPpres, aes_string(x="FPpres", y=i))
  PLOT <- PLOT + geom_jitter(position = position_jitter(width=0.1),alpha=0.4,size=2)
  PLOT <- PLOT + stat_summary(fun.data = "mean_cl_boot", colour = "red",size=1)
  PLOT <- PLOT + xlab("Floating plant presence") + ylab(i)
  PLOT <- PLOT + scale_x_continuous(breaks=c(0,1),labels=c(0,1))
  PLOT <- PLOT + theme_classic(base_size=18)
  PLOT
  
  ggsave(paste("FPpres_vs_",i,"_trans.jpg",sep=""),PLOT,height=4,width=4)
  rm(PLOT)
  
}