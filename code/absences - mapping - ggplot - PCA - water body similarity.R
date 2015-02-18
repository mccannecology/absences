# http://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/

library(ggplot2)
library(maps)
library(maptools) # import for gcDestination
library(grid)
library(scales) # for log2_trans()

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

########################
# prepare a data frame #
########################
data_pca_map <- cbind(data$waterbody, 
                      data$latitude,
                      data$longitude,
                      pca_01$x[,1],
                      pca_01$x[,2]
                  )
 
colnames(data_pca_map) <- c("waterbody","lat","lon","PC1_score","PC2_score")

data_pca_map <- as.data.frame(data_pca_map)

###########################
# A map of surveyed lakes #
###########################
# check that your data frame for the map looks OK 
p <- ggplot()
p <- p + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point(data=data_pca_map, aes(x=lon, y=lat), color="coral1") 
p <- p + theme_classic()
p

#######################
# A map of PC1 scores #
#######################
# to get scale bars source the functions in absence - mapping - ggplot - adding scale bar functions
# these functions from http://editerna.free.fr/wp/?p=76 

map_PC1 <- ggplot()
map_PC1 <- map_PC1 + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey85")
map_PC1 <- map_PC1 + coord_map()
#map_PC1 <- map_PC1 + geom_point(data=data_pca_map, aes(x=lon, y=lat, fill="black"), size=5.5) 
map_PC1 <- map_PC1 + geom_point(data=data_pca_map, aes(x=lon, y=lat, fill=PC1_score),colour="black",pch=21, size=4) 
map_PC1 <- map_PC1 + scale_fill_gradient2(limits=c(-6.25,4.5504),name="PC1 score",low="blue", mid="white", high="red",midpoint=-1.152)
#map_PC1 <- map_PC1 + scale_colour_gradient2(limits=c(-6,6),name="PC1 score",low="blue", mid="white", high="red",midpoint=0)
#map_PC1 <- map_PC1 + ggtitle("") 
map_PC1 <- map_PC1 + theme_classic(base_size=18)
map_PC1 <- map_PC1 + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                                    distanceLon = 25, 
                                    distanceLat = 4, distanceLegend = 9, 
                                    dist.unit = "km", orientation = FALSE,
                                    legend.size = 6)
map_PC1 <- map_PC1 + theme(line = element_blank(),             # get rid of axes 
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(), 
                                 axis.ticks.x = element_blank(),
                                 axis.ticks.y = element_blank(), 
                                 axis.title = element_blank(),
                                 rect = element_blank()
                                 )
map_PC1

ggsave("map_PC1_scores.jpg",map_PC1,height=8,width=11)


#######################
# A map of PC2 scores #
#######################
map_PC2 <- ggplot()
map_PC2 <- map_PC2 + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black",fill="grey85")
map_PC2 <- map_PC2 + coord_map()
#map_PC2 <- map_PC2 + geom_point(data=data_pca_map, aes(x=lon, y=lat, colour=PC2_score), size=4) 
#map_PC2 <- map_PC2 + scale_colour_gradient2(limits=c(-2.75,4.5),name="PC2 score",low="blue", mid="white", high="red",midpoint=0.875)
map_PC2 <- map_PC2 + geom_point(data=data_pca_map, aes(x=lon, y=lat, fill=PC2_score),colour="black",pch=21, size=4) 
map_PC2 <- map_PC2 + scale_fill_gradient2(limits=c(-4.6039,2.6683),name="PC2 score",low="blue", mid="white", high="red",midpoint=0.05)
#map_PC2 <- map_PC2 + ggtitle("") 
map_PC2 <- map_PC2 + theme_classic(base_size=18)
map_PC2 <- map_PC2 + scaleBar(lon = -72.5, lat = 41.,          # add a scale bar
                              distanceLon = 25, 
                              distanceLat = 4, distanceLegend = 9, 
                              dist.unit = "km", orientation = FALSE,
                              legend.size = 6)
map_PC2 <- map_PC2 + theme(line = element_blank(),             # get rid of axes 
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(), 
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(), 
                           axis.title = element_blank(),
                           rect = element_blank()
)
map_PC2

ggsave("map_PC2_scores.jpg",map_PC2,height=8,width=11)

library(gridExtra)
map_PCA_env_2axes <- arrangeGrob(map_PC1,map_PC2)
map_PCA_env_2axes
ggsave("map_PCA_env_2axes.jpg",map_PCA_env_2axes,height=11,width=8)
