###################
## data_prep.R
## Reads in raw aggregate data, subsets by Treatment (sample site), Substrate, Insect
## and Stage. Saves output as .csv files in DataRaw folder.
###################


###################
##Read in raw data
###################
Raw_Data_2019_July<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_July.csv")
Raw_Data_2019_October<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_October.csv")

##################
##Coerce the dates from factor to date in the "Date" column
##################
class(Raw_Data_2019_July$Date)
Raw_Data_2019_July$Date<- as.Date(Raw_Data_2019_July$Date, format="%Y-%m-%d")
Raw_Data_2019_October$Date<- as.Date(Raw_Data_2019_October$Date, format="%Y-%m-%d")
class(Raw_Data_2019_July$Date)
attributes(Raw_Data_2019_July$Treatment)

##################
##Subset by Treatment (Treatment = Site), Substrate (Benthic, Submerged Wood), and Larval insects only
##################

##Subsetting July 2019 Data
Disturbed_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Flooded_Forest_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Flooded Forest" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled
Relic_Channel_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Phase3_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Phase 3") ##Only benthic substrate was sampled
Phase4_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Phase 4") ##Only benthic substrate was sampled
Disturbed_Submerged_Wood_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")
Relic_Channel_Submerged_Wood_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")

##Subsetting October 2019 Data
Disturbed_Benthic_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Flooded_Forest_Benthic_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Flooded Forest" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled
Relic_Channel_Benthic_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Phase3_Benthic_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Phase 3") ##Only benthic substrate was sampled
Phase4_Benthic_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Phase 4") ##Only benthic substrate was sampled
Disturbed_Submerged_Wood_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")
Relic_Channel_Submerged_Wood_Data_2019_October<- subset(Raw_Data_2019_October, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")