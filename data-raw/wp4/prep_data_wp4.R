# Top level script to prepare WP4 data.  Run this script to generate the data structures necessary for the app to run.
# This file sources the scripts to prepare the data for each of the sub-regions.

source("data-raw/wp4/prep_data_NS.R")
cat("NS data prepared\n")
source("data-raw/wp4/prep_data_WW.R")
cat("WW data prepared\n")
source("data-raw/wp4/prep_data_med.R")
cat("MED data prepared\n")
