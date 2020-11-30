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

dummy_benth_clean2 <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/dummy_benth_clean2.csv"
  )
head(dummy_benth_clean2)
##################################################################
## apply stratified() and tapply()in a loop, output as a list,
## 
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
###################################################################
## rbind output list of means, coerce into dataframe, 
## add a column named "site", write to csv
###################################################################

annual_means <- do.call(rbind, means_list)
annual_benth_means <- as.data.frame(annual_means)

site <- rep("alpha", nrow(annual_benth_means))
annual_benth_means$site <- cbind(site)

write.csv(
  annual_benth_means,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_bootdistr_annual_means_alpha.csv",
  row.names = F
)

####################################################################
## combine list of bootstrap sample sets into single object, save
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
quants_of_bootdistro_of_means <- as.data.frame(quants_of_bootdistro_of_means)
## add a column of rownames to pivot by
quants_of_bootdistro_of_means <- rownames_to_column(quants_of_bootdistro_of_means, var = "rowname")

long <- pivot_longer(quants_of_bootdistro_of_means, 0)
####################################################################
## Write tables of means and 95%CI's of bootstrap distribution of 
## annual means to csv files in DataDerived folder
####################################################################

write.csv(
  means_of_bootdistro_of_means,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_means_of_bootdistrib_of_annual_mean_biomasses2.csv",
  row.names = F
)

write.csv(
  quants_of_bootdistro_of_means,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_quantiles_of_bootdistrib_of_annual_mean_biomasses2.csv",
  row.names = F
)
