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

seasonal_biomass <- column_to_rownames(seasonal_biomass, var = "X")

# log transform using decostand
seasonal_biomass_log <- decostand(seasonal_biomass, "log", na.rm = T)

# Standardize by proportion of row totals:
# Divide each value in each row by the row total (total biomass)
# so that each taxa biomass value becomes a proportion of total B
seasonal_biomass_logged_transformed <-
  decostand(seasonal_biomass_log, "total")

################################################
### NMDS analysis
###
################################################

biomass_NMDS <-
  metaMDS(
    seasonal_biomass_logged_transformed,
    distance = "bray",
    k = 2,
    trymax = 500,
    wascores = T,
    
  )

## Create stressplot of results to see how well represented the original
## variables are by the new, reduced variables
stressplot(biomass_NMDS)

###################################
## Graph biplot of reaches
###################################

## create a blank plot

ordiplot(
  biomass_NMDS,
  type = "n",
  xlim = c(-0.6, 0.6),
  ylim = c(-0.6, 0.6),
  main = "Reaches"
)


## add sites/reaches to the plot
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1,
         air = 0.1)


######################################
## Graph biplot of intrinsic species
## intrinsic = drives the pattern of
## distibution of the reaches
######################################


## find vectors of Intrinsic Variables and p-values
intrinsics <-
  envfit(biomass_NMDS,
         seasonal_biomass_logged_transformed,
         permutations = 999)

## create a blank plot

ordiplot(biomass_NMDS, type = "n", main = "Intrinsic Taxa p<0.001")

## plot the sites as points
#orditorp(biomass_NMDS, display = "sites", labels = F, cex = 2)
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1,
         air = 0.07)

## then the intrinsic variables by selecting desired p-value


plot(intrinsics,
     p.max = 0.001,
     col = "black",
     cex = 0.6) # change the significance level of species shown with p.max



##################################
# Scrap Code
#######################

## add species to the plot
ordilabel(
  biomass_NMDS,
  display = "species",
  font = 2,
  priority = priSpp,
  scaling = scl
)

## side-by-side biplots of sites and species separately

layout(matrix(1:2, ncol = 2))
plot(biomass_NMDS, type = "n", scaling = scl)
ordilabel(
  biomass_NMDS,
  display = "sites",
  font = 3,
  fill = "hotpink",
  col = "blue",
  priority = priSite,
  scaling = scl
)
plot(biomass_NMDS, type = "n", scaling = scl)
ordilabel(
  biomass_NMDS,
  display = "species",
  font = 2,
  priority = priSpp,
  scaling = scl
)
layout(1)

orditorp(biomass_NMDS,
         display = "species",
         col = "red",
         air = 0.01)
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1.25,
         air = 0.5)



## biplot results
plot(biomass_NMDS, type = "t")
