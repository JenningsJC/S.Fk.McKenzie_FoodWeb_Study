###################
## data_prep.R
## Reads in raw .csv files saved from DataRaw folder.
## Adds a column for "Season". Rbinds all seasons together.
## Coerces "Date" column from factor to date.
## Removes columns not neededfor calculating mean biomass/year.
## Subsets by Treatment (sample site),Substrate, Insect and Stage. 
## Saves cleaned raw data as .csv files in DataDerived.
###################


###################
## Read in raw data
#
raw_benth_2019_jul<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_jul.csv")
raw_benth_2019_oct<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_oct.csv")
raw_benth_2020_feb<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_feb.csv")
raw_benth_2020_may<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_may.csv")

################################
## Add A Column named "Season" to each seasonal data set
# 
Season <- rep("Summer19",length(raw_benth_2019_jul$Date))
raw_benth_2019_jul$Season<- cbind(Season)

Season <- rep("Fall19",length(raw_benth_2019_oct$Date))
raw_benth_2019_oct$Season<- cbind(Season)

Season <- rep("Winter20",length(raw_benth_2020_feb$Date))
raw_benth_2020_feb$Season<- cbind(Season)

Season <- rep("Spring20",length(raw_benth_2020_may$Date))
raw_benth_2020_may$Season<- cbind(Season)

################
## Rbind the dataframes together
#
raw_benth_data_all_seasons_2019_2020<- rbind(raw_benth_2019_jul,raw_benth_2019_oct,raw_benth_2020_feb,raw_benth_2020_may)

############################################################
## Coerce dates from factor to date in the "Date" column
#
class(raw_benth_data_all_seasons_2019_2020$Date)
raw_benth_data_all_seasons_2019_2020$Date<- as.Date(raw_benth_data_all_seasons_2019_2020$Date, format="%Y-%m-%d")
class(raw_benth_data_all_seasons_2019_2020$Date)

################################
## Removes the columns: Waterbody, Higher.classification,Common.name, Abundance  
#
raw_benth_data_all_seasons_2019_2020<- raw_benth_data_all_seasons_2019_2020[,-c(1,10,13,14)]


##################
## Subset by Treatment (Treatment = Site), Substrate (Benthic, Submerged Wood), 
## Stage and Insect
#
disturbed_benth_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
flood_forest_benth_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Flooded Forest" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled
relic_chan_benth_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
phase3_benth_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Phase 3" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase3
phase4_benth_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Phase 4" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase4
disturbed_wood_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")
relic_chan_wood_ins_clean_data_2019_2020<- subset(raw_benth_data_all_seasons_2019_2020, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")

#########################
###Restoring observations of zero biomass missing from the datasets
#########################

###############
## Pivots by the Replicate column, fills in the biomass values, replaces NA's
## with zeroes

library(tidyr)
disturbed_benth_wider<- disturbed_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

flood_forest_benth_wider<- flood_forest_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

########
## Pivots the individual replicate columns (1-5) back to long format, 
## putting them back into "Replicate" and "Biomass" columns

disturbed_benth_longer<- disturbed_benth_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

flood_forest_benth_longer<- flood_forest_benth_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

