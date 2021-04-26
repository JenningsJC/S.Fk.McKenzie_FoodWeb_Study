library(splitstackshape)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(data.table)

daily_water_temps <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/daily_water_temps_below_dam_2019_2020.csv")

max_temps <- setDT(daily_water_temps)[ , .SD[which.max(temp)], by = date]
min_temps <- setDT(daily_water_temps)[ , .SD[which.min(temp)], by = date] 

Max_mins <- left_join(max_temps, min_temps, by = "date" )
