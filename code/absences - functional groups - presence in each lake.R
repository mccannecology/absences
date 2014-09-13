load("C:/Users/Mike/Desktop/Dropbox/absences/workspace with all CT data - not a lot of clutter.RData")

#  this is the list of species and their functional group 
dataSPECIES_freq3 <- read.csv("dataSPECIES_freq3_with_func_group_labels.csv")
head(dataSPECIES_freq3)

# re-assign it to a new data frame 
data_func_groups <- dataSPECIES_freq3 
rm(dataSPECIES_freq3)

# remove unncessary columns
data_func_groups$frequency <- NULL
data_func_groups$Label <- NULL
data_func_groups$FP. <- NULL

# here's what it looks like now 
head(data_func_groups)


# this data frame has the presence/absences for each water body
colnames(data)
# these are the specific comlumns
colnames(data)[36:152]

# I figured out which columns belong to which functional groups in Excel
# "dataSPECIES_freq3_with_func_group_labels - for merging.csv"
data$emergent_richness <- rowSums(data[,c(36:37,43,47,52:54,57,60,62:74,76,90,97:102,127:134,137:138)])
data$lily_richness <- rowSums(data[,c(38,91:96,136)])
data$submerged_richness <- rowSums(data[,c(39:42,44:46,48:51,55:56,58,59,61,75,77:89,103:126,135,139:152)])

# these could be used for making histograms of the richness of each functional group
table(data$emergent_richness)
table(data$lily_richness)
table(data$submerged_richness)

# the % of water bodies w/o each functional group 
(sum(data$emergent_richness==0) / nrow(data))*100
(sum(data$lily_richness==0) / nrow(data))*100
(sum(data$submerged_richness==0) / nrow(data))*100

