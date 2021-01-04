#########################################################
## Code for sorting the bootstrapping results for making
## graphs
##
##########################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

#########################################################
## Read in files
###########################################################

disturb_benth_quantiles <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/annual_meanquant_bootdistr_disturbed_benth.csv")
bootdistro_disturb_benth_annual_means <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_disturbed_benth.csv")
