# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/

library(ggplot2)
library(maps)
library(maptools) # import for gcDestination
library(grid)

#load us map data
all_states <- map_data("state")

# subset the states you want to see 
states <- subset(all_states, region %in% c("connecticut"))

#####################
# A blank map of CT #
#####################
p <- ggplot()
p <- p + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black", fill="white" )
p <- p + theme_classic()
p

# need to fix dataSPACE. The longitude should be negative 
dataSPACE$longitude <- dataSPACE$longitude*-1

########################
# prepare a data frame #
########################
# these are the columns that I want from "data" data frame 
data$waterbody 
data$latitude 
data$longitude*-1  
data$lemna_minor
data$spirodela_polyrhiza
data$wolffia_sp
as.numeric(data$FP_presence)-1
data$FP_species_richness

# these are the predicted values from the best glms
predict(best_glm_LM_trans, temp_data_LM, type="response")
predict(best_glm_SP_trans, temp_data_SP, type="response")
predict(best_glm_W_trans, temp_data_W, type="response")
predict(best_glm_FPrich_trans, temp_data_FPrich, type="response")
predict(best_glm_FPpres_trans, temp_data_FPpres, type="response")

data_glm_map <- cbind(data$waterbody, 
                  data$latitude, 
                  (data$longitude*-1), 
                  data$lemna_minor,
                  data$spirodela_polyrhiza,
                  data$wolffia_sp,
                  (as.numeric(data$FP_presence)-1),
                  data$FP_species_richness,
                  predict(best_glm_LM_trans, temp_data_LM, type="response"),
                  predict(best_glm_SP_trans, temp_data_SP, type="response"),
                  predict(best_glm_W_trans, temp_data_W, type="response"),
                  predict(best_glm_FPrich_trans, temp_data_FPrich, type="response"),
                  predict(best_glm_FPpres_trans, temp_data_FPpres, type="response")
                  )
 
colnames(data_glm_map) <- c("waterbody","lat","lon","LM_obsv","SP_obsv","W_obsv",
                            "FPpres_obsv","FPrich_obsv","LM_pred","SP_pred","W_pred",
                            "FPrich_pred","FPpres_pred")

data_glm_map <- as.data.frame(data_glm_map)

###########################
# A map of surveyed lakes #
###########################
p <- ggplot()
p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point( data=data_glm_map, aes(x=lon, y=lat), color="coral1") 
p <- p + theme_classic()
p

###################
# A map of LM glm #
###################
# to get scale bars source the functions in absence - mapping - ggplot - adding scale bar functions
# these functions from http://editerna.free.fr/wp/?p=76 

map_LM_glm <- ggplot()
map_LM_glm <- map_LM_glm + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey15")
map_LM_glm <- map_LM_glm + coord_map()
map_LM_glm <- map_LM_glm + geom_point(data=data_glm_map, aes(x=lon, y=lat, colour=LM_pred, shape=as.factor(LM_obsv)), size=4) 
map_LM_glm <- map_LM_glm + scale_colour_gradient2(limits=c(0,1),name="Predicted",low="#FF0000", mid="white", high="#22FF00",midpoint=0.5)
map_LM_glm <- map_LM_glm + scale_shape_discrete(name="Observed",labels=c("absent","present"))
map_LM_glm <- map_LM_glm + ggtitle("Lemna minor") 
map_LM_glm <- map_LM_glm + theme_classic(base_size=18)
map_LM_glm <- map_LM_glm + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                    distanceLon = 25, 
                                    distanceLat = 4, distanceLegend = 6, 
                                    dist.unit = "km", orientation = FALSE)
map_LM_glm <- map_LM_glm + theme(line = element_blank(),             # get rid of axes 
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(), 
                                 axis.ticks.x = element_blank(),
                                 axis.ticks.y = element_blank(), 
                                 axis.title = element_blank(),
                                 rect = element_blank()
                                 )
map_LM_glm

ggsave("map_LM_glm.jpg",map_LM_glm,height=8,width=11)



###################
# A map of SP glm #
###################
map_SP_glm <- ggplot()
map_SP_glm <- map_SP_glm + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey15")
map_SP_glm <- map_SP_glm + coord_map()
map_SP_glm <- map_SP_glm + geom_point(data=data_glm_map, aes(x=lon, y=lat, colour=SP_pred, shape=as.factor(SP_obsv)), size=4) 
map_SP_glm <- map_SP_glm + scale_colour_gradient2(limits=c(0,1),name="Predicted",low="#FF0000", mid="white", high="#22FF00",midpoint=0.5)
map_SP_glm <- map_SP_glm + scale_shape_discrete(name="Observed",labels=c("absent","present"))
map_SP_glm <- map_SP_glm + ggtitle("Spirodela polyrhiza") 
map_SP_glm <- map_SP_glm + theme_classic(base_size=18)
map_SP_glm <- map_SP_glm + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                    distanceLon = 25, 
                                    distanceLat = 4, distanceLegend = 6, 
                                    dist.unit = "km", orientation = FALSE)
map_SP_glm <- map_SP_glm + theme(line = element_blank(),             # get rid of axes 
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(), 
                                 axis.ticks.x = element_blank(),
                                 axis.ticks.y = element_blank(), 
                                 axis.title = element_blank(),
                                 rect = element_blank()
)
map_SP_glm

ggsave("map_SP_glm.jpg",map_SP_glm,height=8,width=11)


##################
# A map of W glm #
##################
map_W_glm <- ggplot()
map_W_glm <- map_W_glm + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey15")
map_W_glm <- map_W_glm + coord_map()
map_W_glm <- map_W_glm + geom_point(data=data_glm_map, aes(x=lon, y=lat, colour=W_pred, shape=as.factor(W_obsv)), size=4) 
map_W_glm <- map_W_glm + scale_colour_gradient2(limits=c(0,1),name="Predicted",low="#FF0000", mid="white", high="#22FF00",midpoint=0.5)
map_W_glm <- map_W_glm + scale_shape_discrete(name="Observed",labels=c("absent","present"))
map_W_glm <- map_W_glm + ggtitle("Wolffia sp.") 
map_W_glm <- map_W_glm + theme_classic(base_size=18)
map_W_glm <- map_W_glm + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                    distanceLon = 25, 
                                    distanceLat = 4, distanceLegend = 6, 
                                    dist.unit = "km", orientation = FALSE)
map_W_glm <- map_W_glm + theme(line = element_blank(),             # get rid of axes 
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(), 
                                 axis.ticks.x = element_blank(),
                                 axis.ticks.y = element_blank(), 
                                 axis.title = element_blank(),
                                 rect = element_blank()
)
map_W_glm

ggsave("map_W_glm.jpg",map_W_glm,height=8,width=11)


#######################
# A map of FPpres glm #
#######################
map_FPpres_glm <- ggplot()
map_FPpres_glm <- map_FPpres_glm + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey15")
map_FPpres_glm <- map_FPpres_glm + coord_map()
map_FPpres_glm <- map_FPpres_glm + geom_point(data=data_glm_map, aes(x=lon, y=lat, colour=FPpres_pred, shape=as.factor(FPpres_obsv)), size=4) 
map_FPpres_glm <- map_FPpres_glm + scale_colour_gradient2(limits=c(0,1),name="Predicted",low="#FF0000", mid="white", high="#22FF00",midpoint=0.5)
map_FPpres_glm <- map_FPpres_glm + scale_shape_discrete(name="Observed",labels=c("absent","present"))
map_FPpres_glm <- map_FPpres_glm + ggtitle("Floating plant presence") 
map_FPpres_glm <- map_FPpres_glm + theme_classic(base_size=18)
map_FPpres_glm <- map_FPpres_glm + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                  distanceLon = 25, 
                                  distanceLat = 4, distanceLegend = 6, 
                                  dist.unit = "km", orientation = FALSE)
map_FPpres_glm <- map_FPpres_glm + theme(line = element_blank(),             # get rid of axes 
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(), 
                               axis.ticks.x = element_blank(),
                               axis.ticks.y = element_blank(), 
                               axis.title = element_blank(),
                               rect = element_blank()
)
map_FPpres_glm

ggsave("map_FPpres_glm.jpg",map_FPpres_glm,height=8,width=11)


#######################
# A map of FPrich glm #
#######################
map_FPrich_glm <- ggplot()
map_FPrich_glm <- map_FPrich_glm + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey15")
map_FPrich_glm <- map_FPrich_glm + coord_map()
map_FPrich_glm <- map_FPrich_glm + geom_point(data=data_glm_map, aes(x=lon, y=lat, colour=FPrich_pred, shape=as.factor(FPrich_obsv)), size=4) 
map_FPrich_glm <- map_FPrich_glm + scale_colour_gradient2(limits=c(0,3),name="Predicted",low="#FF0000", mid="white", high="#22FF00",midpoint=1.5)
map_FPrich_glm <- map_FPrich_glm + scale_shape_discrete(name="Observed")
map_FPrich_glm <- map_FPrich_glm + ggtitle("Floating plant richness") 
map_FPrich_glm <- map_FPrich_glm + theme_classic(base_size=18)
map_FPrich_glm <- map_FPrich_glm + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                            distanceLon = 25, 
                                            distanceLat = 4, distanceLegend = 6, 
                                            dist.unit = "km", orientation = FALSE)
map_FPrich_glm <- map_FPrich_glm + theme(line = element_blank(),             # get rid of axes 
                                         axis.text.x = element_blank(),
                                         axis.text.y = element_blank(), 
                                         axis.ticks.x = element_blank(),
                                         axis.ticks.y = element_blank(), 
                                         axis.title = element_blank(),
                                         rect = element_blank()
)
map_FPrich_glm

ggsave("map_FPrich_glm.jpg",map_FPrich_glm,height=8,width=11)
