library(vegan)

# data frames available 
dataSPACE 
dataENV 
dataFP 
dataNONFP
dataSPECIES

CCA_ENV_SPECIES <- cca(dataSPECIES ~ dataENV$surfacearea_ha + dataENV$shoreline_development + 
                         dataENV$depth_max_m + dataENV$TOTP_avg + dataENV$PH_avg + 
                         dataENV$COND_avg + dataENV$ALK_avg + dataENV$secchi_avg)
CCA_ENV_SPECIES
plot(CCA_ENV_SPECIES)
