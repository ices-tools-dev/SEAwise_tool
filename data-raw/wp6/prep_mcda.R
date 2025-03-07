## code to prepare `mcda` dataset goes here

source('R/utils_mcda.R')

# Example
time_period <- c("2025-2030", "2035-2040", "2045-2050", "2055-2060")
scenarios <- c("Fmsy_RCP45", "Fmsy_RCP85", "PGY_RCP45", "PGY_RCP85", "SQ_RCP45", "SQ_RCP85")

criteria <- c("ssb", "f", "rbs", "mml", "apex_pred", "employment", "wage", "gva", "rsl", "co2")
sub_criteria <- list(ssb = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'),
                     f = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'))
# criteria <- c("ssb", "f", "employment", "wage", "gva", "rsl", "co2")
# sub_criteria <- c("all")
  # list(ssb = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'),
  #                    f = c('DPS171819', 'HKE1718', 'MUT1718', 'NEP17', 'SOL17'))
mcda <- Mcda(time_period,
             scenarios,
             criteria = criteria,
             sub_criteria = "all",
             options_file = "data-raw/wp6/MCDA_options.json")

mcda_data <- load_mcda_data(mcda, path = "dev/MCDA_SHARED/MCDA_SHARED/data/")

usethis::use_data(mcda_data, overwrite = TRUE)
