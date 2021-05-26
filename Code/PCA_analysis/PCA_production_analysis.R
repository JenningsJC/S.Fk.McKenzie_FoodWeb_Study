##########################################
## PCA Analysis of Community 
##   Production Data
## 
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
## Load production files
#####################################

phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/biomass_phase3_2020.csv")
phase4_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/biomass_phase4_2020.csv")
pretreat_main_channel_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/biomass_pretreat_main_channel_2020.csv")
side_channel_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/biomass_side_channel_2020.csv")
wetted_forest_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/biomass_wetted_forest_2020.csv")

#####################################
## Merge biomass data objects by taxa 
## and fill in the NA's with zeroes
######################################

# fulljoin phase 3 and 4 by taxon
fulljoin_phase3_4 <- full_join(phase3_biomass,phase4_biomass, by = "taxon")

# rename biomass.x and biomass.y columns to 'phase3' & 'phase4'
colnames(fulljoin_phase3_4)[2] = "Phase_3"
colnames(fulljoin_phase3_4)[3] = "Phase_4"

# join phase 3 & 4 with pretreat main channel biomass
fulljoin_ph3_4_main_ch <- full_join(fulljoin_phase3_4, pretreat_main_channel_biomass, by = "taxon" )

#rename 'annual.B' column to 'pretreat main chan'
colnames(fulljoin_ph3_4_main_ch)[4] = "Pretreat_main_ch"

# join phase 3, 4, pretreat main channe,and side channel biomass
ph3_4_main_ch_side_ch <- full_join(fulljoin_ph3_4_main_ch, side_channel_biomass, by = "taxon" )

#rename 'annual.B' column to 'Side chan'
colnames(ph3_4_main_ch_side_ch)[5] = "Side_chan"

# join phase 3, 4, pretreat main chan side chan & wetted forest biomass
ph3_4_main_ch_side_ch_forest <- full_join(ph3_4_main_ch_side_ch, wetted_forest_biomass, by = "taxon" )

#rename 'annual.B' column to 'Side chan'
colnames(ph3_4_main_ch_side_ch_forest)[6] = "Wetted_forest"

# replace NA's with zeroes
ph3_4_main_ch_side_ch_forest[is.na(ph3_4_main_ch_side_ch_forest)] = 0

# transpose to make taxa into columns and reaches into rows
combo_biomass_transposed <- (t(ph3_4_main_ch_side_ch_forest[-1]))

colnames(combo_biomass_transposed) <- ph3_4_main_ch_side_ch_forest[, 1]

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


########### stripping column names from data for PcA
#taxa_list <- colnames(combo_biomass_transposed)
#colnames(combo_biomass_transposed) <- NULL

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