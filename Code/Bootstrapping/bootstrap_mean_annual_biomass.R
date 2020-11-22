#####################################################################
## bootstrap_mean_annual_biomass.R
##
## Script to bootstrap resample sets of seasonal biomasses &
## calculate annual mean biomass estimates
##
#####################################################################
rm(list=ls())

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(tidyr)


dummy_benth_clean <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean.csv"
  )



dummy_benth_clean_wider<- dummy_benth_clean %>%
  pivot_wider(names_from = season, values_from = biomass)

##################################################################
## take a stratified random sample, grouped by character vector of 
## specified columns
##################################################################

install.packages("splitstackshape")
library(splitstackshape)

boot_sample <- stratified(dummy_benth_clean,
                          c("taxon", "season", "biomass"), 1 ,
                          replace = TRUE)

##################################################################
## Calculate mean annual biomass for each taxon, from random samples of
## seasonal replicates
##################################################################

tapply(
  boot_sample$biomass,
  list(boot_sample$taxon),
  mean
)

##################################################################
## apply stratified() in a loop
##################################################################

boot_sample <- data.frame()
for(i in 1:4) {
  Y<- stratified(
    dummy_benth_clean,
    c("taxon", "season", "biomass"), 1 ,
    replace = TRUE
  )

}
