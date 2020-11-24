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

dummy_benth_clean2 <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean2.csv"
  )

##################################################################
## apply stratified() and tapply()in a loop, output as a list,
## rbind into dataframe
##################################################################

biomass_list <- list()
means_list <- list()
for (i in 1:5) {
  random_sample <- stratified(dummy_benth_clean2,
                              c("taxon", "season"),
                              1 ,
                              replace = TRUE)
  biomass_list[[i]] <- random_sample
  
  means <- tapply(random_sample$biomass,
                  list(random_sample$taxon),
                  mean)
  means_list[[i]] <- means
}

annual_means <- do.call(rbind, means_list)
annual_benth_means <- as.data.frame(annual_means)
write.csv(annual_benth_means, "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_boot_distro_annual_means.csv", row.names = T )

bio_boot_samples <- biomass_list %>%
  reduce(left_join, by = c("site", "taxon", "season"))

####################################################################
## compute mean and 95% CI of bootstrap distribution of means
####################################################################

boot_means <- apply(annual_benth_means, 2, mean)
means_of_bootdistro_of_means <- as.data.frame(boot_means)

quantile(annual_benth_means$column, probs = 0.975)
quantile(annual_benth_means$column, probs = 0.025)
