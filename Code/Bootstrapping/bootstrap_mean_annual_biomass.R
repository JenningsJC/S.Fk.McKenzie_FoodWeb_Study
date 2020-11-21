#####################################################################
## bootstrap_mean_annual_biomass.R
##
## Script to bootstrap resample sets of seasonal biomasses &
## calculate annual mean biomass estimates
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
  pivot_wider(names_from = season, values_from = biomass)


# Calculate mean biomass, by season and taxon
tapply(
  dummy_benth_clean$biomass,
  list(dummy_benth_clean$season, dummy_benth_clean$taxon),
  mean
)

#take a stratified sample grouped by character vector of column names
stratified(dummy_benth_clean, )
