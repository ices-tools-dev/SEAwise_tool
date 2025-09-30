# Script to prepare the data for WP3 of the project.
# Requires the following files:
# - finalobject.Rdata
# - Task_3.5_BoB_Demersal_Indicators_CCpe85.csv
# - sms_output.csv
#
# Generates the following data objects:
# - stock_productivity

# The script is organized into the following sections:
# 1. Celtic Seas
# 2. Baltic Sea
# 3. Bob
# 4. Greater North Sea

library(dplyr)
load("data-raw/wp3/finalobject.Rdata")
stock_productivity <- final
stock_productivity$wide <- NULL
rm(final)
# standardize names
names(stock_productivity)[names(stock_productivity) == "gns"] <- "greater_north_sea"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Stock"] <- "stock"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Scenario"] <- "scenario"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Indicator"] <- "indicator"

# 1. Celtic Seas - remove baseline scenario and standardize names
stock_productivity$celtic_seas$data <- stock_productivity$celtic_seas$data[stock_productivity$celtic_seas$data$indicator %in% c("catch", "rec", "ssb", "f"),]
stock_productivity$celtic_seas$data <- stock_productivity$celtic_seas$data %>% 
  filter(scenario != "baseline")

stock_productivity$celtic_seas$data$scenario[stock_productivity$celtic_seas$data$scenario=="status quo"] <- "No Climate Change"


# 2. Baltic Sea - remove
stock_productivity$baltic_sea <- NULL



# 3. Bob - Convert "NA" to NA, convert b and f to numeric. Add CCpe85 for Hake.

stock_productivity$bay_of_biscay$refs[stock_productivity$bay_of_biscay$refs == "NA"] <- NA
stock_productivity$bay_of_biscay$refs <- mutate(stock_productivity$bay_of_biscay$refs, b = as.numeric(b), f = as.numeric(f))


bob_rcp85 <- read.csv("data-raw/wp3/Task_3.5_BoB_Demersal_Indicators_CCpe85.csv")[,-1]
bob_rcp85$stock <- "hke.27.3a46-8abd"
bob_rcp85$scenario <- "CCpe85_DD"

stock_productivity$bay_of_biscay$data <- rbind(stock_productivity$bay_of_biscay$data, bob_rcp85)

# 4. Greater North Sea - standardize names
lookup <- c("noCC" = "No Climate Change",
            "rcp45" = "RCP4.5",
            "rcp85" = "RCP8.5")

stock_productivity$greater_north_sea$data$scenario <- unname(lookup[stock_productivity$greater_north_sea$data$scenario])

#  Separate management and climate scenarios
stock_productivity <- purrr::map(stock_productivity, function(region) {
  region$data <- region$data %>%
    mutate(management_scenario = "FMSY") %>%
    mutate(climate_scenario = scenario)
  return(region)
})

# Load SMS data
sms <- read.csv("data-raw/wp3/sms_output.csv")
sms <- sms %>% rename('recruitment' = 'rec', 'f' = 'FI', "catch" = "Yield") %>% 
  tidyr::pivot_longer(c('ssb','recruitment','catch','f'), names_to = "indicator")
sms <- sms %>% mutate(management_scenario = stringr::str_split_i(scenario ,pattern = "_", 1),
               climate_scenario = stringr::str_split_i(scenario ,pattern = "_", 2)) %>% 
  filter(!management_scenario == "ICES-AR")

stock_productivity$greater_north_sea$data <- list("flbeia" = stock_productivity$greater_north_sea$data,
                                                  "sms" = sms)


usethis::use_data(stock_productivity, overwrite = TRUE)

