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
Disturbed_BenthInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Flooded_Forest_BenthInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Flooded Forest" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled
Relic_Channel_BenthInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Phase3_BenthInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Phase 3" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase3
Phase4_BenthInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Phase 4" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase4
Disturbed_WoodInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")
Relic_Channel_WoodInsect_Data_2019<- subset(CombRawDat_2019, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")



