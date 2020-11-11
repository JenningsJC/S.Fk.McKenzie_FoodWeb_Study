###################
## data_prep.R
## Reads in raw .csv files saved from DataRaw folder.
## Adds a column for "Season". Removes columns that are not needed
## for calulating annual mean biomass.
## Subsets by Treatment (sample site),Substrate, Insect and Stage. 
## Rbinds by Treatment. Outputs cleaned data to be saved as .csv in DataDerived.
###################


###################
## Read in raw data
#
raw_benth_2019_jul<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_jul.csv")
raw_benth_2019_oct<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_oct.csv")
raw_benth_2020_feb<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_feb.csv")
raw_benth_2020_may<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_may.csv")
################################
## Adds A Column named "Season" to each seasonal data set
# 
Season <- rep("Summer",length(Raw_Data_2019_July$Date))
Raw_Data_2019_July$Season<- cbind(Season)

Season <- rep("Fall",length(Raw_Data_2019_October$Date))
Raw_Data_2019_October$Season<- cbind(Season)

################
## Rbind the dataframes together
#
CombRawDat_2019<- rbind(Raw_Data_2019_July,Raw_Data_2019_October)

############################################################
## Coerce the dates from factor to date in the "Date" column
#
class(CombRawDat_2019$Date)
CombRawDat_2019$Date<- as.Date(CombRawDat_2019$Date, format="%Y-%m-%d")
class(CombRawDat_2019$Date)

################################
## Removes the columns: Waterbody, Higher.classification,Common.name, Abundance  
#
CombRawDat_2019<- CombRawDat_2019[,-c(1,10,13,14)]


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

#################
## Find the Taxa that are not present in every replicate, and add rows
## in those replicates with zero biomass
#


