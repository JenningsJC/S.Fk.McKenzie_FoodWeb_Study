#####################################################################
## Sort bootstrap distributions of means for each taxa into useful
## groupings. Plot the groupings in barplots
##
#####################################################################

rm(list = ls())

library(tictoc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(forcats)
library(coxed)

##############################################################
## Read-in taxa traits
##############################################################

taxa_traits <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/wisseman_taxa_traits_regress_coeff_2020.csv"
)

####################################################################
## subset the traits by functional feeding groups to use for grouping

piercer_herbivores <- subset.data.frame(taxa_traits, Feeding.Group == "PH")
collector_gatherers <- subset.data.frame(taxa_traits, Feeding.Group == "CG")
collector_filterers <- subset.data.frame(taxa_traits, Feeding.Group == "CF")
predators <- subset.data.frame(taxa_traits, Feeding.Group == "PR")
scrapers <- subset.data.frame(taxa_traits, Feeding.Group == "SC")
shredders <- subset.data.frame(taxa_traits, Feeding.Group == "SH")
omnivores <- subset.data.frame(taxa_traits, Feeding.Group == "OM")
parasites <- subset.data.frame(taxa_traits, Feeding.Group == "PA")


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

###########################################################################
## create a vector of names for each functional feeding group

piercer_herbivore_taxa <- piercer_herbivores$Taxon
collector_gatherer_taxa <- collector_gatherers$Taxon
collector_filterer_taxa <- collector_filterers$Taxon
predator_taxa <- predators$Taxon
scraper_taxa <- scrapers$Taxon
shredder_taxa <- shredders$Taxon
omnivore_taxa <- omnivores$Taxon
parasite_taxa <- parasites$Taxon

###########################################################################
# subsample the bootstrap distributions by feeding groups
# 

disturb_benth_omnivores <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(omnivore_taxa))

disturb_benth_piercer_herbivores <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(piercer_herbivore_taxa))

disturb_benth_collector_gatherers <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(collector_gatherer_taxa))

disturb_benth_collector_filterers <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(collector_filterer_taxa))

disturb_benth_predators <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(predator_taxa))

disturb_benth_scrapers <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(scraper_taxa))

disturb_benth_shredders <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(shredder_taxa))

disturb_benth_parasites <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(parasite_taxa))


############################################################################
## Sum across taxa columns, for all 10k rows. Results are
## stored in a new column called "sum", the bootstrap
## distribution of total mean annual biomass for each feeding group
############################################################################

total_means_disturbed_benth_omnivores <-
  disturb_benth_omnivores %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_omnivores)]))

total_means_disturbed_piercer_herbivores <-
  disturb_benth_piercer_herbivores %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_piercer_herbivores)]))

total_means_disturbed_collector_gatherers <-
  disturb_benth_collector_gatherers %>% mutate(sume = rowSums(.[1:ncol(disturb_benth_collector_gatherers)]))

total_means_disturbed_benth_collector_filterers <-
  disturb_benth_collector_filterers %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_collector_filterers)]))

total_means_disturbed_benth_predators <-
  disturb_benth_predators %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_predators)]))

total_means_disturbed_benth_scrapers <-
  disturb_benth_scrapers %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_scrapers)]))

total_means_disturbed_benth_shredders <-
  disturb_benth_shredders %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_shredders)]))

total_means_disturbed_benth_parasitess <-
  disturb_benth_parasites %>% mutate(sum = rowSums(.[1:ncol(disturb_benth_parasites)]))

##################################################################
# calculate the means of the distributions of annual mean biomasses
# for each feeding group

disturb_benth_omnivores_mean <-
  mean(total_means_disturbed_benth_omnivore[["sum"]])

####################################################################
## Calculate the 95% CI's using the percentile method.
## Add rownames as a column, and pivot longer so it can be rbound
## to the rest of the quantiles, and merged with the table of means
## called "tot_mass" below

# quantiles for percentile method
quants <- c(0.975, 0.025)

#DISTURBED BENTHIC
disturb_benth_omnivores_quantiles <-
  as.data.frame(apply(total_means_disturbed_benth_omnivore[, 3, drop = F], 2 , quantile , probs = quants))

disturb_benth_omni_quants <-
  rownames_to_column(disturb_benth_omnivores_quantiles, var = "rowname")
disturb_benth_omni_quants <-
  pivot_wider(disturb_benth_omni_quants, names_from = rowname, values_from = sum)
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

# combine treatment column with  the means column, and keep mean values numeric by
# using cbind.data.frame

means_quantiles_all_treatments$Treatment <- c(Treatment)

means_quantiles_all_treatments <-
  means_quantiles_all_treatments %>%
  relocate(Treatment, .before = Mean)
