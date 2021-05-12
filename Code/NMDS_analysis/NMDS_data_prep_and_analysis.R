#####################################################################
## Non-Metric Multidimensional Scaling Community Analysis of Annual
## Biomass & Production
## 
## Script to combine the biomass and production means by taxon,
## and format them as a matrix with taxa as columns and reach/
## habitat as rows.
#####################################################################
rm(list = ls())

install.packages("vegan")
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rlang)
library(bestNormalize)

#####################################
## Load biomass files
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
# ph3_4_main_ch_side_ch_forest[is.na(ph3_4_main_ch_side_ch_forest)] = 0

# transpose to make taxa into columns and reaches into rows
combo_biomass_transposed <- (t(ph3_4_main_ch_side_ch_forest[-1]))

colnames(combo_biomass_transposed) <- ph3_4_main_ch_side_ch_forest[, 1]

#############################################
# Write matrix of square root transformed biomass
# to a csv file
#############################################
write.csv(
  combo_biomass_transposed,
  "~/S.Fk.McKenzie_FoodWeb_Study/Code/NMDS_analysis/combined_sqrt_biomass.csv",
  row.names = T
)

write.csv(
  ph3_4_main_ch_side_ch_forest,
  "~/S.Fk.McKenzie_FoodWeb_Study/Code/NMDS_analysis/biomass_ph3_4_main_ch_side_ch_forest.csv",
  row.names = F
)

###############################################
## Transform & Standardize biomass data
##############################################

##combo_biomass_norm <- decostand(combo_biomass_transposed, 'total', na.rm = T)

# square root transform the data
combo_biomass_sqrt<- sqrt(combo_biomass_transposed)

# replace NA's with zeroes
combo_biomass_sqrt[is.na(combo_biomass_sqrt)] = 0

#Wisconsin double-standardization
combo_biomass_wisc <- wisconsin(combo_biomass_sqrt)

################################################
## Convert into Curtis-Bray distance matrix
################################################
biomass_dist_matrix <- vegdist(combo_biomass_wisc, method = "bray")

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
    maxit = 999,
    trymax = 500,
    wascores = TRUE
  )

####



bioNMS <- monoMDS(biomass_dist_matrix, k = 2, model = "global",  threshold = 0.8, maxit = 1000, weakties = TRUE, stress = 1, scaling = TRUE, pc = TRUE, smin = 1e-4, sfgrmin = 1e-7,  sratmax=0.99999) 
goodness(bioNMS)
stressplot(bioNMS)

