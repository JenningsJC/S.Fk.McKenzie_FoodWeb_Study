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
library(purrr)

dummy_benth_clean <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean.csv"
  )

##################################################################
## apply stratified() and tapply()in a loop, output as a list,
## rbind into dataframe
##################################################################

biomass_list <- list()
means_list <- list()
for (i in 1:5) {
  random_sample <- stratified(dummy_benth_clean,
                              c("taxon", "season", "biomass"),
                              1 ,
                              replace = TRUE)
  biomass_list[[i]] <- random_sample
  
  means <- tapply(random_sample$biomass,
                  list(random_sample$taxon),
                  mean)
  means_list[[i]] <- means
}

boot_means <- do.call(rbind, means_list)

bio_df <- biomass_list %>%
  reduce(left_join, by = c("site", "taxon", "season"))
