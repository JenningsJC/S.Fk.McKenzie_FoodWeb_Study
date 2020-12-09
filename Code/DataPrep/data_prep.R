###################################################################
## data_prep.R
## Reads in raw .csv files saved from DataRaw folder.
## Adds a column for "Season". 
## Rbinds all seasons together.
## Coerces "Date" column from factor to date.
## Removes columns not needed for calculating mean biomass/year.
## Subsets by ,Substrate,
## Origin(Aquatic only) & Stage = larvae, pupae, and 
## unknown (because non-insect taxa are all stage=unkown). 
## Restores missing observations of zero biomass by replicate, 
## and season. Saves prepped raw data as .csv files
##  in DataClean folder.
###################################################################
## clear global environment of variables, levels, etc.
rm(list=ls())
###################################################################
## Read in raw data
###################################################################
raw_2019_jul <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_jul.csv"
  )
raw_2019_oct <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2019_oct.csv"
  )
raw_2020_feb <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_feb.csv"
  )
raw_2020_may <-
  read.csv(
    "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/south_fk_mckenzie_benthic_raw_2020_may.csv"
  )

###################################################################
## Add A Column named "Season" to each seasonal data set
###################################################################
Season <- rep("Summer19", length(raw_2019_jul$Date))
raw_2019_jul$Season <- cbind(Season)

Season <- rep("Fall19", length(raw_2019_oct$Date))
raw_2019_oct$Season <- cbind(Season)

Season <- rep("Winter20", length(raw_2020_feb$Date))
raw_2020_feb$Season <- cbind(Season)

Season <- rep("Spring20", length(raw_2020_may$Date))
raw_2020_may$Season <- cbind(Season)

####################################################################
## Rbind the dataframes together
####################################################################
raw_dat_allseasons_2019_2020 <-
  rbind(raw_2019_jul, raw_2019_oct, raw_2020_feb, raw_2020_may)

####################################################################
## Coerce dates from factor to date in the "Date" column
####################################################################
class(raw_dat_allseasons_2019_2020$Date)
raw_dat_allseasons_2019_2020$Date <-
  as.Date(raw_dat_allseasons_2019_2020$Date, format =
            "%Y-%m-%d")
class(raw_dat_allseasons_2019_2020$Date)

#####################################################################
## Write the combined dataset as csv to DataRaw folder
#####################################################################

write.csv(
  raw_dat_allseasons_2019_2020,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_benth_dat_allseasons_2019_2020.csv",
  row.names = F
)

#####################################################################
## Subset by Substrate (Benthic, Submerged Wood), exclude non-aquatic,
## exclude adult stage. Rationale = substrates have different sample
## protocols
#####################################################################

raw_benth_dat_allseasons_2019_2020 <- subset(
  raw_dat_allseasons_2019_2020,
  Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "L" | Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "P" | Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "U"
  
)

raw_wood_dat_allseasons_2019_2020 <- subset(
  raw_dat_allseasons_2019_2020,
  Substrate == "Submerged Wood" &
    Origin == "Aquatic" &
    Stage == "L" | Substrate == "Submerged Wood" &
    Origin == "Aquatic" &
    Stage == "P" | Substrate == "Submerged Wood" &
    Origin == "Aquatic" &
    Stage == "U"
)

## subset the rows excluded from above to make sure
## the number of rows in all the subsets add up
## to the number of rows in raw_dat_allseasons_2019_2020

raw_terrestrial_dat_allseasons_2019_2020 <- subset(
  raw_dat_allseasons_2019_2020,
  Origin == "Terrestrial"
)

raw_adult_dat_allseasons_2019_2020 <- subset(
  raw_dat_allseasons_2019_2020,
  Stage == "A"
)

######################################################################
## Pivot wider each sample site dataset by Stage & Biomass.
## Sum the larvae, pupae, and unknown biomass estimates into a single
## column called "sumrow". Rename "sumrow" to "biomass".Delete columns
## "L", "P", "U"
######################################################################
library(tidyr)
library(dplyr)

##disturbed benthic subset
disturbed_benth_wider_by_stage <-
  disturbed_benth_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

disturbed_benth_wider_summed_biomass <-
  disturbed_benth_wider_by_stage %>% mutate(sumrow = L + P + U)

disturbed_benth_wider_summed_biomass$biomass <-
  disturbed_benth_wider_summed_biomass$sumrow

disturbed_benth_wider_summed_biomass <-
  disturbed_benth_wider_summed_biomass[, -c(11:14)]

##flooded forest benthic subset
flood_forest_benth_wider_by_stage<-
  flood_forest_benth_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

flood_forest_benth_wider_summed_biomass <-
  flood_forest_benth_wider_by_stage %>% mutate(sumrow = L + P + U)

flood_forest_benth_wider_summed_biomass$biomass <-
  flood_forest_benth_wider_summed_biomass$sumrow

flood_forest_benth_wider_summed_biomass <-
  flood_forest_benth_wider_summed_biomass[, -c(11:14)]

##relic channel benthic subset
relic_chan_benth_wider_by_stage<-
  relic_chan_benth_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

relic_chan_benth_wider_summed_biomass <-
  relic_chan_benth_wider_by_stage %>% mutate(sumrow = L + P + U)

relic_chan_benth_wider_summed_biomass$biomass <-
  relic_chan_benth_wider_summed_biomass$sumrow

relic_chan_benth_wider_summed_biomass <-
  relic_chan_benth_wider_summed_biomass[, -c(11:14)]

##phase 3 benthic subset
phase3_benth_wider_by_stage<-
  phase3_benth_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

phase3_benth_wider_summed_biomass <-
  phase3_benth_wider_by_stage %>% mutate(sumrow = L + P + U)

phase3_benth_wider_summed_biomass$biomass <-
  phase3_benth_wider_summed_biomass$sumrow

phase3_benth_wider_summed_biomass <-
  phase3_benth_wider_summed_biomass[, -c(11:14)]

##phase 4 benthic subset
phase4_benth_wider_by_stage<-
  phase4_benth_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

phase4_benth_wider_summed_biomass <-
  phase4_benth_wider_by_stage %>% mutate(sumrow = L + P + U)

phase4_benth_wider_summed_biomass$biomass <-
  phase4_benth_wider_summed_biomass$sumrow

phase4_benth_wider_summed_biomass <-
  phase4_benth_wider_summed_biomass[, -c(11:14)]

##disturbed wood subset
disturbed_wood_wider_by_stage<-
  disturbed_wood_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

disturbed_wood_wider_summed_biomass <-
  disturbed_wood_wider_by_stage %>% mutate(sumrow = L + P + U)

disturbed_wood_wider_summed_biomass$biomass <-
  disturbed_wood_wider_summed_biomass$sumrow

disturbed_wood_wider_summed_biomass <-
  disturbed_wood_wider_summed_biomass[, -c(11:14)]

##relic channel wood subset
relic_chan_wood_wider_by_stage<-
  relic_chan_wood_raw_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

relic_chan_wood_wider_summed_biomass <-
  relic_chan_wood_wider_by_stage %>% mutate(sumrow = L + P + U)

relic_chan_wood_wider_summed_biomass$biomass <-
  relic_chan_wood_wider_summed_biomass$sumrow

relic_chan_wood_wider_summed_biomass <-
  relic_chan_wood_wider_summed_biomass[, -c(11:14)]

######################################################################
## Restore the missing observations of zero biomass to Replicates
##
## Pivots by the Replicate column, fills in the biomass values, replaces NA's
## with zeroes
######################################################################

disturbed_benth_wider_by_replicate<- disturbed_benth_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

flood_forest_benth_wider_by_replicate<- flood_forest_benth_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

relic_chan_benth_wider_by_replicate<- relic_chan_benth_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

phase3_benth_wider_by_replicate<- phase3_benth_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

phase4_benth_wider_by_replicate<- phase4_benth_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

disturbed_wood_wider_by_replicate<- disturbed_wood_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

relic_chan_wood_wider_by_replicate<- relic_chan_wood_wider_summed_biomass %>%
  pivot_wider(names_from = Replicate, values_from = biomass, values_fill = 0)

#######################################################################
## Pivots the individual replicate columns (1-5 for benthic, 1-3 for wood) 
## back to long format, putting them back into "Replicate" and "Biomass" columns
#######################################################################
disturbed_benth_zero_reps<- disturbed_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:14)

flood_forest_benth_zero_reps<- flood_forest_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:14)

relic_chan_benth_zero_reps<- relic_chan_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:14)

phase3_benth_zero_reps<- phase3_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 10:14)

phase4_benth_zero_reps<- phase4_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:14)

disturbed_wood_zero_reps<- disturbed_wood_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:12)

relic_chan_wood_zero_reps<- relic_chan_wood_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "biomass", 10:12)

#######################################################################
## By taxon, restore any missing seasons of replicates and fill w/zeroes
##
## Expand generates a new dataframe consisting of combos of Season & Replicate, by
## Taxon, that are not present in the dataframe. The dataframe with
## the missing seasons is then left-joined back into the original.
#######################################################################

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))

dist_benth_left_join <-
  left_join(miss_seasons_dist_benth,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_flood_forest_benth <-
  expand(flood_forest_benth_zero_reps, Taxon, nesting(Season, Replicate))

flood_forest_benth_left_join <-
  left_join(miss_seasons_flood_forest_benth,
            flood_forest_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))
## Then the missing combos are left-joined back into the original dataframe
disturbed_benth_left_join <-
  left_join(missing_seasons,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))
## Then the missing combos are left-joined back into the original dataframe
disturbed_benth_left_join <-
  left_join(missing_seasons,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))
## Then the missing combos are left-joined back into the original dataframe
disturbed_benth_left_join <-
  left_join(missing_seasons,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))
## Then the missing combos are left-joined back into the original dataframe
disturbed_benth_left_join <-
  left_join(missing_seasons,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))

miss_seasons_dist_benth <-
  expand(disturbed_benth_zero_reps, Taxon, nesting(Season, Replicate))
## Then the missing combos are left-joined back into the original dataframe
disturbed_benth_left_join <-
  left_join(missing_seasons,
            disturbed_benth_zero_reps,
            by = c("Taxon", "Replicate", "Season"))


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
