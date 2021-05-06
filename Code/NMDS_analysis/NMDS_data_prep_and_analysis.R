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

phase3_wider <- phase3_biomass %>%
  pivot_wider(names_from = taxon, values_from = annual.B, values_fill = 0)

phase4_wider <- 

merge_test <- merge(phase3_biomass, phase4_biomass)

# fulljoin phase 3 and 4 by taxon
fulljoin_phase3_4 <- full_join(phase3_biomass,phase4_biomass, by = "taxon")

# rename biomass.x and biomass.y columns to 'phase3' & 'phase4'
colnames(fulljoin_phase3_4)[2] = "Phase_3"
colnames(fulljoin_phase3_4)[3] = "Phase_4"

# join phase 3 & 4 with pretreat main channel biomass
fulljoin_ph3_4_main_ch <- full_join(fulljoin_phase3_4, pretreat_main_channel_biomass, by = "taxon" )


# replace NA's with zeroes
DF1[is.na(DF1)] = 0



set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions