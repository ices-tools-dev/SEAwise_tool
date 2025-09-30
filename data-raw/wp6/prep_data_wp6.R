# Top level script to prepare the data for WP6 of the project.
# Sources the scripts to prepare the data for management strategy evaluation and
# multi-criteria decision analysis.



source("data-raw/wp6/prep_mcda.R")
cat("MCDA data prepared\n")
source("data-raw/wp6/initial_prep_mse_data.R")
cat("MSE data prepared\n")
