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
tot_disturb_benth_quantiles <-
  as.data.frame(apply(total_means_disturbed_benth[, 230, drop = F], 2 , quantile , probs = quants))

#DISTURBED BENTHIC
disturb_benth <-
  rownames_to_column(tot_disturb_benth_quantiles, var = "rowname")
disturb_benth <-
  pivot_wider(disturb_benth, names_from = rowname, values_from = sum)

#DISTURBED WOOD
disturb_wood_quantiles <-
  apply(total_means_disturbed_wood[,154, drop=F ], 2, quantile, probs = quants)

#FLOODED FOREST BENTHIC
tot_floodforest_benth_quantiles <-
  apply(total_means_floodforest_benth [,230, drop=F ], 2 , quantile , probs = quants)

#PHASE3 BENTHIC
tot_phase3_benth_quantiles <-
  apply(total_means_phase3_benth [,230, drop=F ], 2 , quantile , probs = quants)

#PHASE4 BENTHIC
tot_phase4_benth_quantiles <-
  apply(total_means_phase4_benth [,230, drop=F ], 2 , quantile , probs = quants)

#RELICCHAN BENTH
tot_relic_chan_benth_quantiles <-
  apply(total_means_relic_chan_benth [,230, drop=F ], 2 , quantile , probs = quants)

#RELICCHAN WOOD
tot_relicchan_wood_quantiles <-
  apply(total_means_relicchan_wood [, 154, drop = F], 2, quantile, probs = quants)

tot_quantiles <- rbind.data.frame(
  disturb_benth,
  disturb_wood,
  floodforest_benth,
  phase3_benth,
  phase4_benth,
  relic_chan_benth,
  relicchan_wood_quantiles
)

########################################################
# Merge the means and quantiles into a single dataframe

# merge the means into a single vector
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

# create a vector of Treatments for the means
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

tot_mass <- cbind.data.frame(Treatment, Mean)
