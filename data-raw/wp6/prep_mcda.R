# Script to prepare the data for multi-criteria decision analysis.

# The script loads required functions, defines the time period, scenarios, criteria and sub-criteria,
# creates the MCDA object, loads the data into the MCDA object and saves the data to the data folder.

source('R/utils_mcda.R')


time_period <- c("2025-2030", "2035-2040", "2045-2050", "2055-2060")
scenarios <- c("Fmsy_RCP45", "Fmsy_RCP85", "PGY_RCP45", "PGY_RCP85", "SQ_RCP45", "SQ_RCP85")

criteria <- c("ssb", "f", "rbs", "mml", "apex_pred", "employment", "wage", "gva", "rsl", "co2")
sub_criteria <- list(ssb = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'),
                     f = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'))

mcda <- Mcda(time_period,
             scenarios,
             criteria = criteria,
             sub_criteria = "all",
             options_file = "data-raw/wp6/MCDA_options.json")

mcda_data <- load_mcda_data(mcda, path = "data-raw/wp6/")

usethis::use_data(mcda_data, overwrite = TRUE)
