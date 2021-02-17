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
## Read-in bootdistro_means for each Treatment/sample site
## Read-in 
##############################################################

taxa_traits <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/wisseman_taxa_traits_regress_coeff_2020.csv"
)

##csubset the traits by functional feeding groups to use for grouping

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

#######################################################
## create a vector of names for each functional feeding group

piercer_herbivore_taxa <- piercer_herbivores$Taxon
collector_gatherer_taxa <- collector_gatherers$Taxon
collector_filterer_taxa <- collector_filterers$Taxon
predator_taxa <- predators$Taxon
scraper_taxa <- scrapers$Taxon
shredder_taxa <- shredders$Taxon
omnivore_taxa <- omnivores$Taxon
parasite_taxa <- parasites$Taxon

# use the vectors of names of feeding groups to select the bootstrap
# distributions

disturb_benth_omnivores <-
  bootdistr_annual_mean_disturbed_benth %>% 
  select(one_of(omnivore_taxa))

# sum the rows to get bootstrap distribution of estimated annual means
# for each feeding group
