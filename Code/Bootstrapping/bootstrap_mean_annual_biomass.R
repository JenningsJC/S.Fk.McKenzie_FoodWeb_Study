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
library(tibble)

################################################################
## Read in datasets for analysis
#################################################################

disturbed_wood_ <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean3.csv"
  )
Test_Dummy1 <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/Test_Dummy1.csv"
)

##################################################################
## apply stratified() and tapply()in a loop, output as a list,
## 
##################################################################

biomass_list <- list()
means_list <- list()
for (i in 1:3) {
  random_sample <- stratified(real_test_set1,
                              c("Taxon", "Season"),
                              1 ,
                              replace = TRUE)
  biomass_list[[i]] <- random_sample
  
  means <- tapply(random_sample$Biomass,
                  list(random_sample$Taxon),
                  mean)
  means_list[[i]] <- means
}
###################################################################
## rbind output list of means, coerce into dataframe, 
## add a column named "site", write to csv
###################################################################

annual_means <- do.call(rbind, means_list)
annual_benth_means <- as.data.frame(annual_means)

site <- rep("bravo", nrow(annual_benth_means))
annual_benth_means$site <- cbind(site)

write.csv(
  annual_benth_means,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_bootdistr_annual_means_bravo.csv",
  row.names = F
)

####################################################################
## combine list of bootstrap sample sets into single dataframe
## for inspection and error checking
####################################################################

bio_boot_samples <- biomass_list %>%
  reduce(left_join, by = c("site", "taxon", "season"))

####################################################################
## compute mean and 95% CI of bootstrap distribution of annual means
####################################################################

boot_means <- apply(annual_means, 2, mean)
means_of_bootdistro_of_means <- as.data.frame(boot_means)

quants <- c(0.975, 0.025)
quants_of_bootdistro_of_means <-
  apply(annual_means , 2 , quantile , probs = quants)
quants_of_bootdistro_of_means <-
  as.data.frame(quants_of_bootdistro_of_means)

####################################################################
## Create tables of means and 95%CI's of bootstrap distribution of 
## annual means. Combine into one. Write to csv file in 
## DataDerived folder
####################################################################

## add the quantile rownames as a column and rename "quantiles"
quants_of_bootdistro_of_means <-
  rownames_to_column(quants_of_bootdistro_of_means, var = "rowname")
quants_of_bootdistro_of_means <-
  quants_of_bootdistro_of_means %>%
  rename(quantiles = rowname
  )

## add the bootmeans rownames as a column and rename "taxon"
means_of_bootdistro_of_means <-
  rownames_to_column(means_of_bootdistro_of_means, var = "rowname")
means_of_bootdistro_of_means <-
  means_of_bootdistro_of_means %>%
  rename(taxon = rowname
  )
## rename boot_mean column to means
means_of_bootdistro_of_means <-
  means_of_bootdistro_of_means %>%
  rename(mean = boot_means
  )
## extract each list of bounds, remove "quantiles" column, pivot longer,
## and merge with means_of_bootdistro_of_means to make single table

upper_quant <-
  (subset(quants_of_bootdistro_of_means, quantiles == "97.5%"))
upper_quant <- upper_quant[,-c(1)]

lower_quant <-
  (subset(quants_of_bootdistro_of_means, quantiles == "2.5%"))
lower_quant <- lower_quant[,-c(1)]

upper_quant <-
  pivot_longer(upper_quant, 1:3, names_to = "taxon", values_to = "97.5")
lower_quant <-
  pivot_longer(lower_quant, 1:3, names_to = "taxon", values_to = "2.5")

mean_quant_bootdistro_of_means <-
  merge(means_of_bootdistro_of_means, upper_quant)
mean_quant_bootdistro_of_means <-
  merge(mean_quant_bootdistro_of_means, lower_quant)

## write the single table containing means and quantiles to  csv

write.csv(
  mean_quant_bootdistro_of_means,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_mean_quant_of_bootdistr_bravo.csv",
  row.names = F
)

