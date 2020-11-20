#####################################################################
## bootstrap_mean_annual_biomass.R
##
## Script to generate bootstrap distributions of mean annual biomass
##
#####################################################################
rm(list=ls())

dummy_benth_clean <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean.csv"
  )

library(dplyr)
library(tidyr)

dummy_benth_clean_wider<- dummy_benth_clean %>%
  pivot_wider(names_from = replicate, values_from = biomass)


