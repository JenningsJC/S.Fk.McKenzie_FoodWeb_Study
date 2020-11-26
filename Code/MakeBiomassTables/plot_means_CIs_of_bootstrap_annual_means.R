###############################################################
##  Plot means and 95% CI's of bootstrap distribution of estimated
##  mean annual biomasses
###############################################################
rm(list = ls())

library(ggplot2)

boot_dist_annualbiomass1 <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_boot_distrib_annual_mean_biomasses.csv"
)
boot_dist_annualbiomass1 <- subset(boot_dist_annualbiomass1, select = -c(X))
site <- rep("alpha", nrow(boot_dist_annualbiomass1))
boot_dist_annualbiomass1$site <- cbind(site)

boot_dist_annualbiomass2 <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_boot_distrib_annual_mean_biomasses2.csv"
)
boot_dist_annualbiomass2 <- subset(boot_dist_annualbiomass2, select = -c(X))
site <- rep("bravo", nrow(boot_dist_annualbiomass2))
boot_dist_annualbiomass2$site <- cbind(site)

boxplot(
  boot_dist_annualbiomass1,
  boot_dist_annualbiomass2,
  main = "alpha sample site versus bravo",
  ylab = "mean annual biomass"
)
