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

phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/NMDS_data/biomass_phase3_2020.csv")
phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/NMDS_data/biomass_phase3_2020.csv")
phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/NMDS_data/biomass_phase3_2020.csv")
phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/NMDS_data/biomass_phase3_2020.csv")
phase3_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/NMDS_data/biomass_phase3_2020.csv")









set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions