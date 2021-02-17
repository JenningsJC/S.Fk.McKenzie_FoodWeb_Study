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

##create lists of functional feeding groups to use for grouping

piercer_herbivores <- subset.data.frame(taxa_traits, Feeding.Group == "PH")
collector_gatherers <- subset.data.frame(taxa_traits, Feeding.Group == "CG")
collector_filterers <- subset.data.frame(taxa_traits, Feeding.Group == "CF")
predators <- subset.data.frame(taxa_traits, Feeding.Group == "PR")
scrapers <- subset.data.frame(taxa_traits, Feeding.Group == "SC")
shredders <- subset.data.frame(taxa_traits, Feeding.Group == "SH")
omnivores <- subset.data.frame(taxa_traits, Feeding.Group == "OM")
parasites <- subset.data.frame(taxa_traits, Feeding.Group == "PA")
