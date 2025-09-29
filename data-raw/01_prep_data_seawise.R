# Top level script to prepare SEAwise data. Run this script to generate the data structures necessary for the app to run.
# Sources scripts specific to individual work packages as well as scripts preparing data for the app generally.


# Prep WP2
source("data-raw/seawise_colours.R")
cat("Seawise colours prepared\n")

source("data-raw/prep_landing_texts.R")
cat("Landing texts prepared\n")

source("data-raw/prep_figure_texts.R")
cat("Figure texts prepared\n")

source("data-raw/prep_scenario_texts.R")
cat("Scenario texts prepared\n")

source("data-raw/wp2/prep_data_wp2.R")
cat("WP2 data prepared\n")

source("data-raw/wp3/prep_data_wp3.R")
cat("WP3 data prepared\n")

source("data-raw/wp4/prep_data_wp4.R")
cat("WP4 data prepared\n")

#source("data-raw/wp5/prep_data_wp5.R")
#cat("WP5 data prepared\n")

source("data-raw/wp6/prep_data_wp6.R")
cat("WP6 data prepared\n")