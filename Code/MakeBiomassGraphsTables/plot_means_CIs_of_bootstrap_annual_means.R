###############################################################
##  Plot bootstrap distribution of estimated
##  mean annual biomasses by taxa and site.
##  Make tables of means and confidence intervals.
###############################################################
rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
boot_dist_annl_bioms_alpha <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_bootdistr_annual_means_alpha.csv"
)

boot_dist_annl_bioms_bravo <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_bootdistr_annual_means_bravo.csv"
)

## prep means data for boxplotting by pivoting longer
boot_dist_alpha_long <- pivot_longer(boot_dist_annl_bioms_alpha, 1:3, names_to = "taxon", values_to = "means")
## coerce "taxon" column from characters to factors
boot_dist_alpha_long$taxon <- as.factor(boot_dist_alpha_long$taxon)

## create boxplots of boot distributions of annual means
plot1 <- ggplot(boot_dist_alpha_long, aes(x=taxon, y=means, color=taxon)) + geom_boxplot()
plot1 + stat_summary(fun=mean, geom="point", shape=1, size=3)

