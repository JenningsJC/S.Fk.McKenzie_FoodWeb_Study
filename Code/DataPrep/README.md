This folder contains file(s) for scripts to process raw benthic data into "clean" data files for bootstrapping:

data_prep.R = takes the raw seasonal biomass .csv files (extracted from "Long Aggregate" tab in Wisseman's Excel files), removes unnecessary columns, adds a column for season, and rbinds them by sample site into single "clean" files consisting of a complete annual(2019-2020) data set.