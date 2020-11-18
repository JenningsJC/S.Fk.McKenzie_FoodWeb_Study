#############################################################################
## data_prep.R
## Reads in raw .csv files saved from DataRaw folder.
## Adds a column for "Season". 
## Rbinds all seasons together.
## Coerces "Date" column from factor to date.
## Removes columns not needed for calculating mean biomass/year.
## Subsets by sample site("Treatment"),Substrate, Insect(insects only),
## Origin(Aquatic only) & Stage (larvae & pupae). 
## Restores missing observations of zero biomass by replicate, and season.
## Saves prepped raw data as .csv files in DataClean folder.
#############################################################################


###################################################################
## Read in raw data
###################################################################
raw_2019_jul<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_jul.csv")
raw_2019_oct<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_oct.csv")
raw_2020_feb<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_feb.csv")
raw_2020_may<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_may.csv")

###################################################################
## Add A Column named "Season" to each seasonal data set
###################################################################
Season <- rep("Summer19",length(raw_benth_2019_jul$Date))
raw_2019_jul$Season<- cbind(Season)

Season <- rep("Fall19",length(raw_benth_2019_oct$Date))
raw_2019_oct$Season<- cbind(Season)

Season <- rep("Winter20",length(raw_benth_2020_feb$Date))
raw_2020_feb$Season<- cbind(Season)

Season <- rep("Spring20",length(raw_benth_2020_may$Date))
raw_2020_may$Season<- cbind(Season)

####################################################################
## Rbind the dataframes together
####################################################################
raw_data_all_seasons_2019_2020<- rbind(raw_2019_jul,raw_2019_oct,raw_2020_feb,raw_2020_may)

####################################################################
## Coerce dates from factor to date in the "Date" column
####################################################################
class(raw_data_all_seasons_2019_2020$Date)
raw_data_all_seasons_2019_2020$Date<- as.Date(raw_data_all_seasons_2019_2020$Date, format="%Y-%m-%d")
class(raw_data_all_seasons_2019_2020$Date)

#####################################################################
## Removes the columns: Waterbody, Higher.classification,Common.name, Abundance  
#####################################################################
raw_data_trim_all_seasons_2019_2020<- raw_data_all_seasons_2019_2020[,-c(1,10,13,14)]


#####################################################################
## Subset by Treatment (Treatment = Site), Substrate (Benthic, Submerged Wood), 
## Stage, Origin and Insect
#####################################################################
disturbed_benth_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P")
flood_forest_benth_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Flooded Forest" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P") ##Only benthic substrate was sampled
relic_chan_benth_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P")
phase3_benth_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Phase 3" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P") ##Only benthic substrate was sampled in Phase3
phase4_benth_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Phase 4" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P") ##Only benthic substrate was sampled in Phase4
disturbed_wood_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P")
relic_chan_wood_ins_raw_2019_2020<- subset(raw_data_trim_all_seasons_2019_2020, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Origin=="Aquatic" & Stage=="L" | Stage=="P")

######################################################################
### Restore the missing observations of zero biomass to Replicates
######################################################################

######################################################################
### Pivots by the Replicate column, fills in the biomass values, replaces NA's
## with zeroes
######################################################################
library(tidyr)

disturbed_benth_ins_wider<- disturbed_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

flood_forest_benth_ins_wider<- flood_forest_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

relic_chan_benth_ins_wider<- relic_chan_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

phase3_benth_ins_wider<- phase3_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

phase4_benth_ins_wider<- phase4_benth_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

disturbed_wood_ins_wider<- disturbed_wood_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

relic_chan_wood_ins_wider<- relic_chan_wood_ins_clean_data_2019_2020 %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

#######################################################################
### Pivots the individual replicate columns (1-5) back to long format, 
## putting them back into "Replicate" and "Biomass" columns
#######################################################################
disturbed_benth_ins_longer<- disturbed_benth_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

flood_forest_benth_ins_longer<- flood_forest_benth_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

relic_chan_benth_ins_longer<- relic_chan_benth_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

phase3_benth_ins_longer<- phase3_benth_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

phase4_benth_ins_longer<- phase4_benth_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:15)

disturbed_wood_ins_longer<- disturbed_wood_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:13)

relic_chan_wood_ins_longer<- relic_chan_wood_ins_wider %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 11:13)

#######################################################################
### By taxon, restore any missing seasons of replicates an fill w/zeroes
#######################################################################

library(dplyr)
missing_seasons<- expand(disturbed_benth_ins_longer, Taxon, nesting(Season, Replicate))
disturb_left_join <-left_join(missing_seasons, disturbed_benth_ins_longer, by= c("Taxon","Replicate", "Season"))
## Next task is to figure out how to fill in all the NAs with the appropriate
## variable in each column, And check to make sure that the number of rows in
## the completed dataset has the expected number of rows
## The number should be: no. of unique taxa * no. seasons * no. replicates

#######################################################################
### Write the dataframes as .csv to the DataClean folder
#######################################################################

write.csv(disturbed_benth_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/disturbed_benth_ins_clean_2019_2020.csv", row.names = F )

write.csv(flood_forest_benth_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/flood_forest_benth_ins_clean_2019_2020.csv", row.names = F)

write.csv(relic_chan_benth_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/relic_chan_benth_ins_clean_2019_2020.csv", row.names = F)

write.csv(phase3_benth_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/phase3_benth_ins_clean_2019_2020.csv", row.names = F)

write.csv(phase4_benth_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/phase4_benth_ins_clean_2019_2020.csv", row.names = F)

write.csv(disturbed_wood_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/disturbed_wood_ins_clean_2019_2020.csv", row.names = F)

write.csv(relic_chan_wood_ins_longer, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/relic_chan_wood_ins_clean_2019_2020.csv", row.names = F)
