#####################################################################
## Non-Metric Multidimensional Scaling Community Analysis of Seasonal
## Mean Biomass 
## 
#####################################################################
rm(list = ls())

install.packages("vegan")
install.packages("factoextra")
install.packages("ecodist")
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rlang)
library(bestNormalize)
library(factoextra)
library(ecodist)
#####################################
## Load biomass file
#####################################

seasonal_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/seasonal_mean_biomass_benth_all_reaches.csv")


###############################################
## Transform & Standardize biomass data
##############################################

# log transform using decostand
seasonal_biomass_log<- decostand(seasonal_biomass, "log")

# Standardize by proportion of row totals:
# Divide each value in each row by the row total (total biomass)
# so that each taxa biomass value becomes a proportion of total B
seasonal_biomass_log_props <- decostand(seasonal_biomass_log, "total")

################################################
## Convert into Curtis-Bray distance matrix
################################################
biomass_dist_matrix <- vegdist(seasonal_biomass_log_props, method = "bray")

biomass_dist_matrix <- as.matrix(biomass_dist_matrix, labels = T)


################################################
### NMDS with combined biomass matrix
###
################################################

  biomass_NMDS <-
  metaMDS(
    biomass_dist_matrix,
    distance = "bray",
    k = 3,
    maxit = 10000,
    trymax = 5000,
    wascores = TRUE,
    
  )

####



bioNMS <- monoMDS(biomass_dist_matrix, k = 2, model = "global",  threshold = 0.7, maxit = 1000, weakties = TRUE, stress = 1, scaling = TRUE, pc = TRUE, smin = 1e-4, sfgrmin = 1e-7,  sratmax=0.99999) 
goodness(bioNMS)
stressplot(bioNMS)
bioNMS
