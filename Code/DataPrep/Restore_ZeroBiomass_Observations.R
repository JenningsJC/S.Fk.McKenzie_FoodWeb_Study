Raw_Data_2020_winter<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/February 2020 SF McKenzie River.csv")

##############
## Subsetting the data from the Disturbed sample site 

Raw_Biomass_winter_2020<- Raw_Data_2020_winter[,-c(1,10,13,14)]
Disturb_BenthInsect_winter_2020<- subset(Raw_Biomass_winter_2020, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")

#########
## Pivots by the Replicate column, fills in the biomass values, replaces NA's
## with zeroes

library(tidyr)
Dist_Benth_Winter_2020_Wider<- Disturb_BenthInsect_winter_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)
View(Disturb_BenthInsect_winter_2020)

