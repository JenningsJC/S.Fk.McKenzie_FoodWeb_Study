###############################################################
###############################################################
##  Graph boxplots of bootstrap distributions of estimated
##  mean annual biomasses by taxa and site.
##  Make tables of means and confidence intervals.
###############################################################
###############################################################

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

disturb_benth_quantiles <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/annual_meanquant_bootdistr_disturbed_benth.csv"
  )
bootdistro_disturb_benth_annual_means <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/bootdistr_annual_mean_disturbed_benth.csv"
  )


########################################################
## prep bootstrap distribution of means data for 
## boxplotting by pivoting longer
########################################################
## 1:n below, n = number of taxa/columns in data frame
bootdistro_disturbed_benth_long <-
  pivot_longer(bootdistro_disturb_benth_annual_means,
               1:229,
               names_to = "Taxon",
               values_to = "Means")

## coerce "taxon" column from characters to factors
bootdistro_disturbed_benth_long$Taxon <- as.factor(bootdistro_disturbed_benth_long$Taxon)

########################################################
## create boxplots of boot distributions of annual means 
## for each site, by taxon
########################################################
tic()
plot1 <-
  ggplot(bootdistro_disturbed_benth_long, aes(x = Taxon, y = Means, color = Taxon)) + geom_boxplot()
plot1 + stat_summary(
  fun = mean,
  geom = "point",
  shape = 1,
  size = 3
)
toc()

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

boot_means_quants_alpha_bravo <- rbind(boot_means_quants_alpha, boot_means_quants_bravo)

## coerce "site" column from characters to factors
boot_means_quants_alpha_bravo$site <- as.factor(boot_means_quants_alpha_bravo$site)

##rename quantile columns
boot_means_quants_alpha_bravo <- boot_means_quants_alpha_bravo %>%
  rename("97.5" = X97.5,
         "2.5" = X2.5)
#########################################################
## make tables of means % 95% CI's of bootstrap
## distributions of means by taxon and site
#########################################################
table1 <- gt(data = boot_means_quants_alpha_bravo)
table1
table1 <- table1 %>%
  tab_header(
    title = "Estimated Annual Mean Biomass w/ 95% Confidence Intervals",
    subtitle = "By Taxon & Sample Site (mg/m^2)"
  )
table1
