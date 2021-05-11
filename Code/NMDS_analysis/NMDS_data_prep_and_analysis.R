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

########################################
# Square-root transform each reach
########################################

### Phase 3 ###
# Add a column of square root transformed data
phase3_biomass$sqrt.B <- sqrt(phase3_biomass$annual.B)

# Remove untransformed column
phase3_biomass_sqrt <- phase3_biomass[-2]

### Phase 4 ###
# Add a column of square root transformed data
phase4_biomass$sqrt.B <- sqrt(phase4_biomass$annual.B)

# Remove untransformed column
phase4_biomass_sqrt <- phase4_biomass[-2]

### Pre-treatment Main Channel ###
# Add a column of square root transformed data
pretreat_main_channel_biomass$sqrt.B <- sqrt(pretreat_main_channel_biomass$annual_B)

# Remove untransformed column
pretreat_main_channel_biomass_sqrt <- pretreat_main_channel_biomass[-2]

### Side Channel ###
# Add a column of square root transformed data
side_channel_biomass$sqrt.B <- sqrt(side_channel_biomass$annual.B)

# Remove untransformed column
side_channel_biomass_sqrt <- side_channel_biomass[-2]

### Wetted Forest ###
# Add a column of square root transformed data 
wetted_forest_biomass$sqrt.B <- sqrt(wetted_forest_biomass$annual.B)

wetted_forest_biomass_sqrt <- wetted_forest_biomass[-2]

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

#############################################
# Write matrix of biomass means to csv file
#############################################
write.csv(
  combo_biomass_transposed,
  "~/S.Fk.McKenzie_FoodWeb_Study/Code/NMDS_analysis/matrix_of_combined_biomass_means.csv",
  row.names = T
)

write.csv(
  ph3_4_main_ch_side_ch_forest,
  "~/S.Fk.McKenzie_FoodWeb_Study/Code/NMDS_analysis/biomass_ph3_4_main_ch_side_ch_forest.csv",
  row.names = F
)



################################################
### NMDS with combined biomass matrix
###
################################################

biomass_NMDS=metaMDS(combo_biomass_transposed, zerodist = "add", maxit = 100
                    , try = 200, trymax = 100,
                     k=2) # k = The number of reduced dimensions, trymax = more iterations than default



stressplot(biomass_NMDS)

