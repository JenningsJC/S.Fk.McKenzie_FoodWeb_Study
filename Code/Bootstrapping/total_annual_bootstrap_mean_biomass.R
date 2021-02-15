##############################################################
##  Derive Bootstrap Distribution of Total Annual Biomass per 
##  unit area: Sum each row of the boostrap distribution of 
##  mean biomass by taxon, compute mean & 95% CIs of boot 
##  distribution of total biomass for each sample site
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
## Sum across taxa columns, for all 10k rows. Results are
## stored in a new column called "sum", which is the boot
## distribution of total annual biomass for the sample site
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
  bootdistr_annual_mean_relic_chan_benth %>% mutate(sum = rowSums(.[1:229]))

total_means_relicchan_wood <-
  bootdistr_annual_mean_relicchan_wood %>% mutate(sum = rowSums(.[1:153]))
####################################################################
## compute mean and 95% CI of bootstrap distribution of annual means
####################################################################

#######################
# calculate the means of the distributions of total annual mean biomasses
disturb_benth_mean_total <-
  mean(total_means_disturbed_benth[["sum"]])

disturb_wood_mean_total <-
  mean(total_means_disturbed_wood[["sum"]])

floodforest_benth_mean_total <-
  mean(total_means_floodforest_benth[["sum"]])

phase3_mean_total <-
  mean(total_means_phase3_benth[["sum"]])

phase4_mean_total <-
  mean(total_means_phase4_benth[["sum"]])

relic_chan_benth_mean_total <-
  mean(total_means_relic_chan_benth[["sum"]])

relic_chan_wood_mean_total <-
  mean(total_means_relicchan_wood[["sum"]])

########################3
## Calculate the 95% CI using the percentile method.
## Add rownames as a column, and pivot longer so it can be rbound
## to the resto of the quatiles, and merged with the table of means
## called "tot_mass" below

quants <- c(0.975, 0.025)

#DISTURBED BENTHIC
tot_disturb_benth_quantiles <-
  as.data.frame(apply(total_means_disturbed_benth[, 230, drop = F], 2 , quantile , probs = quants))

disturb_benth <-
  rownames_to_column(tot_disturb_benth_quantiles, var = "rowname")
disturb_benth <-
  pivot_wider(disturb_benth, names_from = rowname, values_from = sum)

#DISTURBED WOOD
disturb_wood_quantiles <-
  as.data.frame(apply(total_means_disturbed_wood[,154, drop=F ], 2, quantile, probs = quants))

disturb_wood <-
  rownames_to_column(disturb_wood_quantiles, var = "rowname")
disturb_wood <-
  pivot_wider(disturb_wood, names_from = rowname, values_from = sum)

#FLOODED FOREST BENTHIC
floodforest_benth_quantiles <-
  as.data.frame(apply(total_means_floodforest_benth [,230, drop=F ], 2 , quantile , probs = quants))

floodforest_benth <-
  rownames_to_column(floodforest_benth_quantiles, var = "rowname")
floodforest_benth <-
  pivot_wider(floodforest_benth, names_from = rowname, values_from = sum)

#PHASE3 BENTHIC
phase3_benth_quantiles <-
  as.data.frame(apply(total_means_phase3_benth [,230, drop=F ], 2 , quantile , probs = quants))

phase3_benth <-
  rownames_to_column(phase3_benth_quantiles, var = "rowname")
phase3_benth <-
  pivot_wider(phase3_benth, names_from = rowname, values_from = sum)

#PHASE4 BENTHIC
phase4_benth_quantiles <-
  as.data.frame(apply(total_means_phase4_benth [,230, drop=F ], 2 , quantile , probs = quants))

phase4_benth <-
  rownames_to_column(phase4_benth_quantiles, var = "rowname")
phase4_benth <-
  pivot_wider(phase4_benth, names_from = rowname, values_from = sum)

#RELICCHAN BENTH
relic_chan_benth_quantiles <-
  as.data.frame(apply(total_means_relic_chan_benth [,230, drop=F ], 2 , quantile , probs = quants))

relic_chan_benth <-
  rownames_to_column(relic_chan_benth_quantiles, var = "rowname")
relic_chan_benth <-
  pivot_wider(relic_chan_benth, names_from = rowname, values_from = sum)

#RELICCHAN WOOD
relicchan_wood_quantiles <-
  as.data.frame(apply(total_means_relicchan_wood [, 154, drop = F], 2, quantile, probs = quants))

relicchan_wood <-
  rownames_to_column(relicchan_wood_quantiles, var = "rowname")
relicchan_wood <-
  pivot_wider(relicchan_wood, names_from = rowname, values_from = sum)

## Rbind the quantiles using rbind.data.frame to keep the values numeric
tot_quantiles <- rbind.data.frame(
  disturb_benth,
  disturb_wood,
  floodforest_benth,
  phase3_benth,
  phase4_benth,
  relic_chan_benth,
  relicchan_wood
)

## merge the means into a single vector
tot_annual_mass_allsites <-
  c(
    disturb_benth_mean_total,
    disturb_wood_mean_total,
    floodforest_benth_mean_total,
    phase3_mean_total,
    phase4_mean_total,
    relic_chan_benth_mean_total,
    relic_chan_wood_mean_total
  )
Mean <- as.numeric(tot_annual_mass_allsites)

## Merge the quantiles and means
tot_quantiles$Mean <- c(Mean)

means_quantiles_all_treatments <-
  tot_quantiles %>%
  relocate(Mean, .before = `97.5%`)

# create a vector of Treatments for the names of means
Treatment <- c(
  "disturb_benth",
  "disturb_wood",
  "floodforest_benth",
  "phase3",
  "phase4",
  "relic_chan_benth",
  "relic_chan_wood"
)

# combine treatment column with  the means column, and keep means numeric values by
# using cbind.data.frame

means_quantiles_all_treatments$Treatment <- c(Treatment)

means_quantiles_all_treatments <-
  means_quantiles_all_treatments %>%
  relocate(Treatment, .before = Mean)

## write the data frame to a csv
write.csv(
  means_quantiles_all_treatments,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/total_means_quantiles_all_treatments.csv"
)
