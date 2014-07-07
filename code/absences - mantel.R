library(ade4)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

# create distance matrices 
SPACEdist <- dist(dataSPACE)
FPdist <- dist(data$FPrichness)

mantel.rtest(SPACEdist,FPdist, nrepet=9999)

# Warning message:
# In is.euclid(m2) : Zero distance(s)

# Monte-Carlo test
# Observation: 0.07800177 
# Call: mantel.rtest(m1 = SPACEdist, m2 = FPdist, nrepet = 9999)
# Based on 9999 replicates
# Simulated p-value: 0.0121  #  reject the null hypothesis that these two matrices are unrelated 