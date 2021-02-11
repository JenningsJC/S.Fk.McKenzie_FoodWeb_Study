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

total_means_disturbed_wood <-
  bootdistr_annual_mean_disturbed_wood %>% mutate(sum = rowSums(.[1:153]))

total_means_floodforest_benth <-
  bootdistr_annual_mean_floodforest_benth %>% mutate(sum = rowSums(.[1:229]))

total_means_phase3_benth <-
  bootdistr_annual_mean_phase3_benth %>% mutate(sum = rowSums(.[1:229]))

total_means_phase4_benth <-
  bootdistr_annual_mean_phase4_benth %>% mutate(sum = rowSums(.[1:229]))

total_means_relic_chan_benth <-
  bootdistr_annual_mean_relic_chane_benth %>% mutate(sum = rowSums(.[1:229]))

total_means_relicchan_wood <-
  bootdistr_annual_mean_relicchan_wood %>% mutate(sum = rowSums(.[1:153]))
####################################################################
## compute mean and 95% CI of bootstrap distribution of annual means
####################################################################

# calculate the mean of the distribution of total mean biomasses
tot_disturb_benth_meanof_distro <-
  mean(total_means_disturbed_benth[["sum"]])






# calculate the 95% CI using the percentile method
quants <- c(0.975, 0.025)
tot_disturb_benth_quantiles <-
  apply(tot_disturb_benth_meanof_distro , 2 , quantile , probs = quants)


quants_of_bootdistro_of_means <-
  as.data.frame(quants_of_bootdistro_of_means)