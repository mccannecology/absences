load("C:/Users/Mike/Desktop/Dropbox/absences/workspace with all CT data - not a lot of clutter.RData")

# multivariate similarity of FP composition for all 174 water bodies 
dist(dataFP)

# matrix of euclidean distances between all points 
dataLATLON <- dataSPACE[,1:2]
dist(dataLATLON)

# this is not very useful... 
# there are few different FP similarities
plot(dist(dataLATLON), dist(dataFP))
hist(dist(dataFP))