library(ade4)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

# create distance matrices 
SPACEdist <- dist(dataSPACE)
FPspdist <- dist(dataFP)
FPrichdist <- dist(data$FPrichness)
SPECIESdist <- dist(dataSPECIES)

mantel.rtest(SPACEdist,FPrichdist, nrepet=9999)
# Monte-Carlo test
# Observation: 0.07800177 
# Call: mantel.rtest(m1 = SPACEdist, m2 = FPdist, nrepet = 9999)
# Based on 9999 replicates
# Simulated p-value: 0.0121  #  reject the null hypothesis that these two matrices are unrelated 
# Warning message:
# In is.euclid(m2) : Zero distance(s)

mantel.rtest(SPACEdist,FPspdist, nrepet=9999)
# Monte-Carlo test
# Observation: 0.06925981 
# Call: mantel.rtest(m1 = SPACEdist, m2 = FPspdist, nrepet = 9999)
# Based on 9999 replicates
# Simulated p-value: 0.0295 
# Warning message:
# In is.euclid(m2) : Zero distance(s)

mantel.rtest(SPACEdist,SPECIESdist, nrepet=9999)
# Monte-Carlo test
# Observation: 0.1460259 
# Call: mantel.rtest(m1 = SPACEdist, m2 = SPECIESdist, nrepet = 9999)
# Based on 9999 replicates
# Simulated p-value: 3e-04 