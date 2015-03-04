# http://stats.stackexchange.com/questions/61217/transforming-variables-for-multiple-regression-in-r

library(car)

formula <- LM ~ surfacearea_ha + shoreline_development + depth_max_m + TOTP_avg + COND_avg + ALK_avg + secchi_avg + waterbodies_1km + 
                  waterbodies_10km + dist_waterfowl + nearest_LM + latitude + (-longitude) + nonFP_species_richness

boxTidwell(formula, other.x=~boatlaunch, data=temp_data_LM)
