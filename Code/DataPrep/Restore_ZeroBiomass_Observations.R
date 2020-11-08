Raw_Data_2020_winter<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/February 2020 SF McKenzie River.csv")


Raw_Biomass_winter_2020<- Raw_Data_2020_winter[,-c(1,10,13,14)]
Disturb_BenthInsect_winter_2020<- subset(Raw_Biomass_winter_2020, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")


library(tidyr)
Dist_Benth_Winter_2020_Wider<- Disturb_BenthInsect_winter_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass)
View(Disturb_BenthInsect_winter_2020)

