########################################################################################
# WHY ARE WATER BODIES WITH A CLOSE OCCUPIED NEIGHBOR LESS LIKELY TO BE OCCUPIED ????? #
# Compare the model with just nearest_LM to the full model                             #
# WHY DOES THE SIGN OF THE COEFF FOR NEAREST_LM SWITCH BETWEEN THESE MODELS?           #
########################################################################################
# water bodies with LM
jpeg(filename="nearest_LM - ponds with FP.jpg")
hist(temp_data_LM$nearest_LM[temp_data_LM$LM==1],xlab="nearest_LM",main="Ponds with FP")
dev.off()
mean(temp_data_LM$nearest_LM[temp_data_LM$LM==1])
summary(temp_data_LM$nearest_LM[temp_data_LM$LM==1])
# water bodies without LM
jpeg(filename="nearest_LM - ponds without FP.jpg")
hist(temp_data_LM$nearest_LM[temp_data_LM$LM==0],xlab="nearest_LM",main="Ponds without FP")
dev.off()
mean(temp_data_LM$nearest_LM[temp_data_LM$LM==0])
summary(temp_data_LM$nearest_LM[temp_data_LM$LM==0])

# plot cond vs. nearest_LM 
# transformed data 
library(ggplot2)
cond_nearest_LM_plot <- ggplot(temp_data_LM, aes(x=nearest_LM,y=COND_avg,colour=as.factor(LM))) + geom_point(size=3)
cond_nearest_LM_plot <- cond_nearest_LM_plot + theme_classic(base_size=18)
cond_nearest_LM_plot <- cond_nearest_LM_plot + scale_colour_manual(name="Lemna minor",values=c("grey55","black"),labels=c("absent","present"))
cond_nearest_LM_plot
ggsave("cond_vs_nearestLM - LM.jpg",cond_nearest_LM_plot,height=8,width=8)

# plot cond vs. nearest_LM 
# raw data 
cond_nearest_LM_plot <- ggplot(data, aes(x=nearest_LM,y=COND_avg,colour=as.factor(lemna_minor))) + geom_point(size=3)
cond_nearest_LM_plot <- cond_nearest_LM_plot + theme_classic(base_size=18)
cond_nearest_LM_plot <- cond_nearest_LM_plot + ylab("Conductivity (us/cm)")
cond_nearest_LM_plot <- cond_nearest_LM_plot + xlab("Nearest L. minor (km)")
cond_nearest_LM_plot <- cond_nearest_LM_plot + scale_colour_manual(name="Lemna minor",values=c("grey55","black"),labels=c("absent","present"))
cond_nearest_LM_plot
ggsave("cond_vs_nearestLM - LM.jpg",cond_nearest_LM_plot,height=8,width=11)




# fit models with different combinations of predictor variables 
temp <- glm(LM ~ nearest_LM, 
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~ surfacearea_ha + shoreline_development + TOTP_avg + PH_avg + COND_avg +
              secchi_avg + waterbodies_1km + waterbodies_10km + dist_waterfowl + 
              nearest_LM + latitude + waterbodies_1km + boatlaunch + nonFP_species_richness,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  TOTP_avg + COND_avg + secchi_avg + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  COND_avg + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  COND_avg * nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  shoreline_development + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  secchi_avg + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  TOTP_avg + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  waterbodies_1km + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

temp <- glm(LM ~  boatlaunch + nearest_LM,
            family=binomial, data=temp_data_LM, na.action = "na.fail")

summary(temp)
