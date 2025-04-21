## code to prepare `stock_productivity` dataset goes here

load("data-raw/wp3/finalobject.Rdata")
stock_productivity <- final
stock_productivity$wide <- NULL
rm(final)
names(stock_productivity)[names(stock_productivity) == "gns"] <- "greater_north_sea"

names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Stock"] <- "stock"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Scenario"] <- "scenario"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Indicator"] <- "indicator"

# Celtic Seas
stock_productivity$celtic_seas$data <- stock_productivity$celtic_seas$data[stock_productivity$celtic_seas$data$indicator %in% c("catch", "rec", "ssb", "f"),]

# BS
stock_productivity$baltic_sea <- NULL





### Bob

stock_productivity$bay_of_biscay$refs[stock_productivity$bay_of_biscay$refs == "NA"] <- NA
stock_productivity$bay_of_biscay$refs <- mutate(stock_productivity$bay_of_biscay$refs, b = as.numeric(b), f = as.numeric(f))


bob_rcp85 <- read.csv("data-raw/wp3/Task_3.5_BoB_Demersal_Indicators_CCpe85.csv")[,-1]
bob_rcp85$stock <- "hke.27.3a46-8abd"
#bob_rcp85_refs <- read.csv("data-raw/wp3/Task_3.5_BoB_Demersal_RefPts_CCpe85.csv")[,-1]
stock_productivity$bay_of_biscay$data <- rbind(stock_productivity$bay_of_biscay$data, bob_rcp85)

# purrr::map(.x = stock_productivity, ~c(unique(.x[[1]]$scenario)))

stock_productivity <- purrr::map(stock_productivity, function(region) {
  region$data <- region$data %>%
    mutate(management_scenario = "FMSY") %>%
    mutate(climate_scenario = scenario)
  return(region)
})

stock_productivity$bay_of_biscay$data

# GNS
sms <- read.csv("data-raw/wp3/sms_output.csv")
sms <- sms %>% rename('recruitment' = 'rec', 'f' = 'FI', "catch" = "Yield") %>% 
  tidyr::pivot_longer(c('ssb','recruitment','catch','f'), names_to = "indicator")
sms <- sms %>% mutate(management_scenario = stringr::str_split_i(scenario ,pattern = "_", 1),
               climate_scenario = stringr::str_split_i(scenario ,pattern = "_", 2)) %>% 
  filter(!management_scenario == "ICES-AR")

stock_productivity$greater_north_sea$data <- list("flbeia" = stock_productivity$greater_north_sea$data,
                                                  "sms" = sms)

usethis::use_data(stock_productivity, overwrite = TRUE)

