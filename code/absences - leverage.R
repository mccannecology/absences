########
# data # (no transformations)
########
load("C:/Users/Mike/Desktop/Dropbox/absences/workspace - all glms dredge - raw.RData")

plot(cooks.distance(glm_LM))
plot(cooks.distance(glm_SP))
plot(cooks.distance(glm_W))
plot(cooks.distance(glm_FPpres))
plot(cooks.distance(glm_FPrich))

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
