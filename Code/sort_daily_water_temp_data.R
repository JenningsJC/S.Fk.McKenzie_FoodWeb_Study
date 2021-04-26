library(splitstackshape)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(data.table)

########################################################################################
## Read in csv file of annual daily temps, which was copied from the tab file from the 
## USGS website, and cleaned up in Excel and saved as a CSV in the DataRaw file
#########################################################################################
daily_water_temps <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/daily_water_temps_below_dam_2019_2020.csv")

########################################################################################
## Use data.table to pull out maximum and minimum temps for each day of the year
######################################################################################
max_temps <- setDT(daily_water_temps)[ , .SD[which.max(temp)], by = date]
min_temps <- setDT(daily_water_temps)[ , .SD[which.min(temp)], by = date] 

##################################
## Join the dataframes by date
###################################
Max_mins <- left_join(max_temps, min_temps, by = "date" )

##########################################
## Write to CSV for further work in Excel
##########################################
write.csv(
  Max_mins,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/max_min_daily_water_temp_south_fork_mckenzie_2019_2020.csv",
  row.names = F
)
