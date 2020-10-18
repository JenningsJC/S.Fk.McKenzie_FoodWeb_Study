##Read in raw data
Raw_Data_2019_July<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_July.csv")

##Coerce all the dates from factor to date in the "Date" column
class(Raw_Data_2019_July$Date)
Raw_Data_2019_July$Date<- as.Date(Raw_Data_2019_July$Date, format="%Y-%m-%d")
class(Raw_Data_2019_July$Date)
attributes(Raw_Data_2019_July$Treatment)

##Subset data by Treatment (Treatment = Site) and Substrate (Benthic, Submerged Wood)
Disturbed_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Disturbed" & Substrate=="Benthic")
Disturbed_Submerged_Wood_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Disturbed" & Substrate=="Submerged Wood")
Flooded_Forest_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Flooded Forest") ##Only benthic substrate was sampled
Relic_Channel_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Relic Floodplain Channel" & Substrate=="Benthic")
Relic_Channel_Submerged_Wood_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Relic Floodplain Channel" & Substrate=="Submerged Wood")
Phase3_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Phase 3") ##Only benthic substrate was sampled
Phase4_Benthic_Data_2019_July<- subset(Raw_Data_2019_July, Treatment=="Phase 4") ##Only benthic substrate was sampled

