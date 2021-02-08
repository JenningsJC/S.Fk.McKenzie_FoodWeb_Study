###############################################################
###############################################################
##  Graph boxplots of bootstrap distributions of estimated
##  mean annual biomasses by taxa and site.
##  Make tables of means and confidence intervals.
###############################################################
###############################################################

rm(list = ls())
library(tictoc)
library(ggplot2)
library(dplyr)
library(tidyr)

disturb_benth_quantiles <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/annual_meanquant_bootdistr_disturbed_benth.csv"
  )




#####################################################
## Prep mean and quantile data for graphing
## tables
#####################################################



## Rename quantile columns
disturb_benth_quantiles <- disturb_benth_quantiles %>%
  rename("97.5" = X97.5,
         "2.5" = X2.5)

## Remove taxa with annual means of zero
disturb_benth_quant_AllZero <- filter(disturb_benth_quantiles, mean == 0)
disturb_benth_quant_NoZero <- filter(disturb_benth_quantiles, mean > 0)

## Remove taxa with 95% CIs that include zero
disturb_benth_quant_bigs<- filter(disturb_benth_quantiles, `2.5` > 0)

## subset by range of mean
upper <- subset(disturb_benth_quant_bigs, mean >= 10, select = c(taxon, mean, `97.5`, `2.5`))

lower <- subset(disturb_benth_quant_bigs, mean <= 10 & mean >= 1, select = c(taxon, mean, `97.5`, `2.5`))

lowest <- subset(disturb_benth_quant_bigs, mean <= 1, select = c(taxon, mean, `97.5`, `2.5`))

############################################################################
## Make bargraphs with error bars showing CI's
##
############################################################################


ggplot(disturb_benth_quant_bigs, mapping = aes(x=taxon, y=mean)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= `2.5`, ymax= `97.5`), width=.2,
                position=position_dodge(.9))


#########################################################
## make tables of means % 95% CI's of bootstrap
## distributions of means by taxon and site
#########################################################

install.packages('gt')
library(gt)

table1 <- gt(data = boot_means_quants_alpha_bravo)
table1
table1 <- table1 %>%
  tab_header(
    title = "Estimated Annual Mean Biomass w/ 95% Confidence Intervals",
    subtitle = "By Taxon & Sample Site (mg/m^2)"
  )
table1

#######################################################
## Read in csv of biitdistro of means
######################################################
bootdistro_disturb_benth_annual_means<-
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


###########################################################
## Make horizontal boxplots of distribution of annual means
##
##########################################################



qplot(Taxon, Means, data = bootdistro_disturbed_benth_long, geom = "boxplot") +
  coord_flip()
temp1 <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_benth_wood_dat_allseasons_2019_2020.csv")
