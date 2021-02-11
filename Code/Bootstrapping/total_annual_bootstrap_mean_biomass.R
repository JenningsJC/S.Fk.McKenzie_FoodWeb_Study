##############################################################
##  Bootstrap Distribution of Total Annual Biomass per 
##  unit area: Sum each row of the boostrap distribution of 
##  mean biomass by taxon
##############################################################

rm(list = ls())

library(tictoc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(forcats)

##############################################################
## Read-in bootdistro_means for each Treatment/sample site
##############################################################

bootdistr_annual_mean_disturbed_benth <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_disturbed_benth.csv"
  )

bootdistr_annual_mean_disturbed_wood <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_disturbed_wood.csv"
)

bootdistr_annual_mean_floodforest_benth <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_floodforest_benth.csv"
  )

bootdistr_annual_mean_phase3_benth <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_phase3_benth.csv"
  )

bootdistr_annual_mean_phase4_benth <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_phase4_benth.csv"
  )

bootdistr_annual_mean_relic_chan_benth <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_relic_chan_benth.csv"
  )

bootdistr_annual_mean_relicchan_wood <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_RelicChan_wood.csv"
  )

##############################################################
## Sum across columns, row by row
##############################################################

total_means_disturbed_benth <-
  bootdistr_annual_mean_disturbed_benth %>% mutate(sum = rowSums(.[1:229]))


####################################################################
## compute mean and 95% CI of bootstrap distribution of annual means
####################################################################

tic()
boot_means <- apply(annual_means, 2, mean)
toc()
means_of_bootdistro_of_means <- as.data.frame(boot_means)

tic()
quants <- c(0.975, 0.025)
quants_of_bootdistro_of_means <-
  apply(annual_means , 2 , quantile , probs = quants)
toc()
quants_of_bootdistro_of_means <-
  as.data.frame(quants_of_bootdistro_of_means)