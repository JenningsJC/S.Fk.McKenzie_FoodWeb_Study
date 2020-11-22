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

means<- tapply(
  boot_sample$biomass,
  list(boot_sample$taxon),
  mean
)
class(means)
##################################################################
## apply stratified() and tapply()in a loop, output as a list,
## rbind into dataframe
##################################################################

biomass_list <- list()
means_list <- list()
for(i in 1:3) {
  random_sample<- stratified(
    dummy_benth_clean,
    c("taxon", "season", "biomass"), 1 ,
    replace = TRUE
    )
  biomass_list[[i]] <- random_sample
  
  means<- tapply(
    random_sample$biomass,
    list(random_sample$taxon),
    mean
  )
  means_list[[i]] <- means
}

boot_means <- do.call(rbind, means_list)
boot_bios <- do.call(cbind, biomass_list)
