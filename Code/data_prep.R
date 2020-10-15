Raw_Data_2019_July<- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/SFMR_Wisseman_Long_Aggregate_Output_2019_July.csv")

##Coerce all the dates from factor to date in the "Date" column
class(Raw_Data_2019_July$Date)
Raw_Data_2019_July$Date<- as.Date(Raw_Data_2019_July$Date, format="%Y-%m-%d")
class(Raw_Data_2019_July$Date)
