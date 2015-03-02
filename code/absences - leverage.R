########
# data # (no transformations)
########
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - raw.RData")

plot(cooks.distance(glm_LM)); points(c(129,142),cooks.distance(glm_LM)[c(129,142)],col="red")
plot(cooks.distance(glm_SP)); points(c(129,142),cooks.distance(glm_SP)[c(129,142)],col="red")
plot(cooks.distance(glm_W)); points(c(129,142),cooks.distance(glm_W)[c(129,142)],col="red")
plot(cooks.distance(glm_FPpres)); points(c(129,142),cooks.distance(glm_FPpres)[c(129,142)],col="red")
plot(cooks.distance(glm_FPrich)); points(c(129,142),cooks.distance(glm_FPrich)[c(129,142)],col="red")

#################
# dataENV_trans # (scaled 0-1 and skewness-reduced)
################# 
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - transformed.RData")

plot(cooks.distance(glm_LM_trans))
plot(cooks.distance(glm_SP_trans))
plot(cooks.distance(glm_W_trans))
plot(cooks.distance(glm_FPpres_trans))
plot(cooks.distance(glm_FPrich_trans))

##################
# dataENV_scaled # (scaled 0-1)
################## 
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - scaled.RData")

plot(cooks.distance(glm_LM_scaled))
plot(cooks.distance(glm_SP_scaled))
plot(cooks.distance(glm_W_scaled))
plot(cooks.distance(glm_FPpres_scaled))
plot(cooks.distance(glm_FPrich_scaled))
