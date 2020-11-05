This folder contains file(s) for scripts to process raw data into "clean" data files for further processing:

data_prep.R = takes the raw seasonal biomass .csv files (extracted from Wisseman's raw seasonal biomass files), removes unnecessary columns, adds a column for season, and rbinds them into one single "clean" file for bootstrap resampling. 