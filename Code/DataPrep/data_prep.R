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
## read libraries in 
library(tidyr)
library(dplyr)
library(forcats)
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

############################################################################
## raw_2019_jul dataset has two names for the same treatment:
## "Relic Channel" & "Relic Floodplain Channel" are the same sample site/
##  Treatment. In the Treatment column, change the factor
## "Relic Floodplain Channel" to "Relic Channel".
#############################################################################
unique(raw_2019_jul$Treatment)

raw_2019_julx <-
  raw_2019_jul %>% 
  mutate(Treatment = fct_recode(Treatment, "Relic Channel" = "Relic Floodplain Channel"))
####################################################################
## Rbind the dataframes together
####################################################################
raw_dat_allseasons_2019_2020 <-
  rbind(raw_2019_julx, raw_2019_oct, raw_2020_feb, raw_2020_may)

####################################################################
## Coerce dates from factor to date in the "Date" column
####################################################################
class(raw_dat_allseasons_2019_2020$Date)
raw_dat_allseasons_2019_2020$Date <-
  as.Date(raw_dat_allseasons_2019_2020$Date, format =
            "%Y-%m-%d")
class(raw_dat_allseasons_2019_2020$Date)

#####################################################################
## Removes the columns: Waterbody, Abundance
#####################################################################
raw_dat_allseasons_2019_2020 <-
  raw_dat_allseasons_2019_2020[, -c(1,14)]

### check Treatment factor levels
unique(raw_dat_allseasons_2019_2020$Treatment)
## combined raw dataset has 5 factor levels for Treatment, as it should
#####################################################################
## Write the combined dataset as csv to DataRaw folder
#####################################################################

write.csv(
  raw_dat_allseasons_2019_2020,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_benth_wood_dat_allseasons_2019_2020.csv",
  row.names = F
)

#####################################################################
## Subset by Substrate (Benthic, Submerged Wood), exclude non-aquatic
## taxa, exclude adult stage. 
## Rationale = substrates have different sample protocols
#####################################################################

raw_benth_allseasons_2019_2020 <- subset(
  raw_dat_allseasons_2019_2020,
  Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "L" | Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "P" | Substrate == "Benthic" &
    Origin == "Aquatic" &
    Stage == "U"
  
)

raw_wood_allseasons_2019_2020 <- subset(
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
## Pivot wider, names from Stage & values from Biomass.
## Sum the larvae, pupae, and unknown biomass values into 
## single column called "biomass". Delete 
## biomass_stage & sumrow columns.
######################################################################


########################################################################################
##1. pivot on the combined benthic subset (all seasons and all treatments/sample sites)
########################################################################################
benth_widerX_by_stage <-
  raw_benth_allseasons_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

# sum the biomass values, by stage
benth_wider_summed_biomass <-
  benth_widerX_by_stage %>% mutate(sumrow = L + P + U)

# add sums as a "Biomass" column
benth_wider_summed_biomass$Biomass <-
  benth_wider_summed_biomass$sumrow

# delete biomass_stage and sumrow columns
benth_wider_summed_biomass <-
  benth_wider_summed_biomass[, -c(13:16)]

#######################################################################################
##2. pivot on combined wood subset (all seasons and all treatments/sample sites)
#######################################################################################

wood_wider_by_stage<-
  raw_wood_allseasons_2019_2020 %>%
  pivot_wider(names_from = Stage,
              values_from = Biomass,
              values_fill = 0)

# sum the biomass values
wood_wider_summed_biomass <-
  wood_wider_by_stage %>% mutate(sumrow = L + P + U)

# add sums as a "Biomass" column
wood_wider_summed_biomass$Biomass <-
  wood_wider_summed_biomass$sumrow

# delete biomass_stage and sumrow columns
wood_wider_summed_biomass <-
  wood_wider_summed_biomass[, -c(13:16)]

#####################################################################
## Write the datasets of combined pupal/larval/unknown
## biomasses as csv file to DataRaw folder
#####################################################################

write.csv(
  benth_wider_summed_biomass,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_benth_combined_stages_allseasons_2019_2020.csv",
  row.names = F
)

write.csv(
  wood_wider_summed_biomass,
  "~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_wood_combined_stages_allseasons_2019_2020.csv",
  row.names = F
)

######################################################################
## Restore the missing observations of zero biomass to Replicates:
##
######################################################################

## read in the csv files from the previous step
raw_wood_combined_stages_allseasons <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_wood_combined_stages_allseasons_2019_2020.csv")

raw_benth_combined_stages_allseasons <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataRaw/raw_benth_combined_stages_allseasons_2019_2020.csv")


######################################################################
## Pivot by the Replicate column, fill in the biomass values, replace NA's
## with zeroes
#######################################################################

raw_benth_wider_by_replicate<- raw_benth_combined_stages_allseasons %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)

raw_wood_wider_by_replicate<- raw_wood_combined_stages_allseasons %>%
  pivot_wider(names_from = Replicate, values_from = Biomass, values_fill = 0)


#######################################################################
## Pivots the individual replicate columns (1-5 for benthic, 1-3 for wood) 
## back to long format, putting them back into "Replicate" and "Biomass" columns
#######################################################################
rawbenth_restored_replicates <- raw_benth_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 12:16)

rawwood_restored_replicates <- raw_wood_wider_by_replicate %>%
  pivot_longer(names_to = "Replicate", values_to = "Biomass", 12:14)



#######################################################################
## By taxon, restore any missing seasons of replicates and fill w/zeroes
##
## Expand generates a new dataframe consisting of all possible combos of 
## Season & Replicate, by Taxon, that are not present in the dataframe. 
## The dataframe with the missing seasons is then left-joined back into the original,
## leaving rows with blanks where missing seasons are present.
#######################################################################

miss_seasons_benth <-
  expand(rawbenth_restored_replicates, Taxon, nesting(Treatment, Season, Replicate))

raw_benth_sxn_left_join <-
  left_join(miss_seasons_benth,
            rawbenth_restored_replicates,
            by = c("Treatment", "Taxon", "Replicate", "Season"))


miss_seasons_wood <-
  expand(rawwood_restored_replicates, Taxon, nesting(Treatment, Season, Replicate))

raw_wood_sxn_left_join <-
  left_join(miss_seasons_wood,
            rawwood_restored_replicates,
            by = c("Treatment", "Taxon", "Replicate", "Season"))


## Next task is to figure out how to fill in all the NAs with the appropriate
## variable in each column, And check to make sure that the number of rows in
## the completed dataset has the expected number of rows
## The number should be: no. of unique taxa * no. seasons * no. replicates

#######################################################################
## Remove the columns not needed for Biomass bootstrapping.
#######################################################################

raw_benth_sxn_left_join <- raw_benth_sxn_left_join[, -c(5:12)]

raw_wood_sxn_left_join <- raw_wood_sxn_left_join[, -c(5:12)]

#######################################################################
## Replace NA's in Biomass column with zeroes.
##
#######################################################################

raw_benth_sxn_left_joinX <-
  raw_benth_sxn_left_join %>% replace_na(list(Biomass = 0))

raw_wood_sxn_left_joinX <-
  raw_wood_sxn_left_join %>% replace_na(list(Biomass = 0))

#######################################################################
### Write prepped dataframes as .csv to the DataClean folder, as 
### backups.
#######################################################################

write.csv(raw_benth_sxn_left_joinX, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/prepped_all_benth_2019_2020.csv", row.names = F )


write.csv(raw_wood_sxn_left_joinX, "~/S.Fk.McKenzie_FoodWeb_Study/DataClean/prepped_all_wood_2019_2020.csv", row.names = F)

#######################################################################
### Subset the benthic and wood surface datasets by Treatment.
#######################################################################

unique(raw_benth_sxn_left_joinX$Treatment)

disturbed_benth_clean <- subset(raw_benth_sxn_left_joinX, Treatment == "Disturbed")

floodForest_benth_clean <- subset(raw_benth_sxn_left_joinX, Treatment == "Flooded Forest")

phase3_benth_clean <- subset(raw_benth_sxn_left_joinX, Treatment == "Phase 3")

phase4_benth_clean <- subset(raw_benth_sxn_left_joinX, Treatment == "Phase 4")

relicChannel_clean <- subset(raw_benth_sxn_left_joinX, Treatment == "Relic Channel")
