##########################################
## PCA Analysis of Seasonal Biomass Means
## by Reach
##########################################

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
## Load seasonal biomass file
#####################################

seasonal_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/seasonal_mean_biomass_benth_all_reaches.csv")

###############################################
## Transform & Standardize biomass data
##############################################

combo_biomass_log<- decostand(combo_biomass_transposed, "log")

# square root transform the data
combo_biomass_sqrt<- sqrt(combo_biomass_transposed)



# Divide each value in each row by the row total (total biomass),
# so that each taxa biomass value becomes a proportion of total B

combo_biomass_props <- decostand(combo_biomass_transposed, "total")
combo_biomass_sqrt_props <- decostand(combo_biomass_sqrt, "total")
combo_biomass_log_props <- decostand(combo_biomass_log, "total")

################################################
##   PCA Analysis
###############################################


######## PcA analysis of untransformed data
PCA_results <- prcomp(combo_biomass_transposed)
biplot(PCA_results) 

####### PcA analysis of raw proportions
PCA_results2 <- prcomp(combo_biomass_props)
biplot(PCA_results2)

####### PcA analysis of proportions of sqrt data
PCA_results3 <- prcomp(combo_biomass_sqrt_props)
biplot(PCA_results3)

####### PCA of proportions of log standardized data
PCA_results4 <- prcomp(combo_biomass_log_props)
biplot(PCA_results4,)



####### extract eigenvalues to select the most important components for plotting
eig.val3 <- get_eigenvalue(PCA_results3)
eig.val4 <- get_eigenvalue(PCA_results4)

####### Graph Individuals (Reaches) so they are grouped together

fviz_pca_ind(PCA_results4,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

####### Graph the highest contributing Variables 

fviz_pca_var(PCA_results4,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

####### Graph PCA biplot

fviz_pca_biplot(PCA_results4, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.var = list(contrib = 30)
)