###################
## data_prep.R
## Reads in raw "long aggregate output" .csv files saved from master spreadsheet.
## Rbinds the files together. 
## Subsets by Treatment (sample site), Substrate, Insect and Stage. 
## Saves the subsets as .csv file in the DataRaw folder.
###################


###################
## Read in raw data
###################
Raw_Data_2019_July<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_July.csv")
Raw_Data_2019_October<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_October.csv")

################################
## Rbind the dataframes together
################################
Raw_Data_2019_Jul_Oct<- rbind(Raw_Data_2019_July,Raw_Data_2019_October)

##################
## Coerce the dates from factor to date in the "Date" column
##################
class(Raw_Data_2019_Jul_Oct$Date)
Raw_Data_2019_Jul_Oct$Date<- as.Date(Raw_Data_2019_Jul_Oct$Date, format="%Y-%m-%d")
class(Raw_Data_2019_Jul_Oct$Date)

##################
## Subset by Treatment (Treatment = Site), Substrate (Benthic, Submerged Wood), 
## Stage and Insect
##################

Disturbed_BenthInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Disturbed" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Flooded_Forest_BenthInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Flooded Forest" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled
Relic_Channel_BenthInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic" & Insect=="insect" & Stage=="L")
Phase3_BenthInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Phase 3" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase3
Phase4_BenthInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Phase 4" & Insect=="insect" & Stage=="L") ##Only benthic substrate was sampled in Phase4
Disturbed_WoodInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Disturbed" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")
Relic_Channel_WoodInsect_Data_2019_Jul_Oct<- subset(Raw_Data_2019_Jul_Oct, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood" & Insect=="insect" & Stage=="L")


