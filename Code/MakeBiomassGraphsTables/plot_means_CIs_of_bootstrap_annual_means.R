###############################################################
##  Graph boxplots of bootstrap distributions of estimated
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
########################################################
## prep bootstrap distribution of means data for 
## boxplotting by pivoting longer
########################################################

boot_dist_alpha_long <-
  pivot_longer(boot_dist_annl_bioms_alpha,
               1:3,
               names_to = "taxon",
               values_to = "means")

boot_dist_bravo_long <-
  pivot_longer(boot_dist_annl_bioms_bravo,
               1:3,
               names_to = "taxon",
               values_to = "means")

## coerce "taxon" column from characters to factors
boot_dist_alpha_long$taxon <- as.factor(boot_dist_alpha_long$taxon)

boot_dist_bravo_long$taxon <- as.factor(boot_dist_bravo_long$taxon)

########################################################
## create boxplots of boot distributions of annual means 
## for each site, by taxon
########################################################
plot1 <-
  ggplot(boot_dist_alpha_long, aes(x = taxon, y = means, color = taxon)) + geom_boxplot()
plot1 + stat_summary(
  fun = mean,
  geom = "point",
  shape = 1,
  size = 3
)

plot2 <-
  ggplot(boot_dist_bravo_long, aes(x = taxon, y = means, color = taxon)) + geom_boxplot()
plot2 + stat_summary(
  fun = mean,
  geom = "point",
  shape = 1,
  size = 3
)
########################################################
## creat boxplots of boot distributions of annual means 
## by taxon, grouped by site
########################################################
boot_dist_alpha_bravo <-
  rbind(boot_dist_alpha_long, boot_dist_bravo_long)

plot3 <-
  ggplot(boot_dist_alpha_bravo, aes(x = taxon, y = means, fill = site)) 
  + geom_boxplot(position =
    position_dodge(1)
)
plot3 + stat_summary(
  position = position_dodge(1),
  fun = mean,
  geom = "point",
  shape = 1,
  size = 3
)
#####################################################
## Prep mean and quantile data for graphing
## tables
#####################################################
install.packages('gt')
library(gt)

boot_means_quants_alpha <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_mean_quant_of_bootdistr_alpha.csv"
)
site <- rep("alpha", nrow(boot_means_quants_alpha))
boot_means_quants_alpha$site <- cbind(site)

boot_means_quants_bravo <- read.csv(
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/dummy_mean_quant_of_bootdistr_bravo.csv"
)
site <- rep("bravo", nrow(boot_means_quants_bravo))
boot_means_quants_bravo$site <- cbind(site)


#########################################################
## make tables of means % 95% CI's of bootstrap
## distributions of means by taxon and site
#########################################################
