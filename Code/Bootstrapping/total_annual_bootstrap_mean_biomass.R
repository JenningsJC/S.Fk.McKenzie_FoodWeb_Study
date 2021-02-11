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

disturb_benth_quantiles <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/annual_meanquant_bootdistr_disturbed_benth.csv"
  )
