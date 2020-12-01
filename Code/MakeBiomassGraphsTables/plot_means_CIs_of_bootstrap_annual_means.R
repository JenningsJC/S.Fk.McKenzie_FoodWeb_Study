###############################################################
##  Plot bootstrap distribution of estimated
##  mean annual biomasses by taxa and site.
##  Make tables of means and confidence intervals.
###############################################################
rm(list = ls())

library(ggplot2)

boot_dist_annl_bioms_alpha <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_boot_distr_annual_means_alpha.csv"
)



boot_dist_annualbiomass2 <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_boot_distr_annual_means_bravo.csv"
)


par(mfcol,c(5,3))
boxplot(
  boot_dist_annualbiomass1,
  boot_dist_annualbiomass2,
  main = "alpha sample site versus bravo",
  ylab = "mean annual biomass"
)
