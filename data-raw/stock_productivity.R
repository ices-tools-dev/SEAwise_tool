## code to prepare `stock_productivity` dataset goes here

load("data-raw/wp3/finalobject.Rdata")
stock_productivity <- final
rm(final)
names(stock_productivity)[names(stock_productivity) == "gns"] <- "greater_north_sea"

names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Stock"] <- "stock"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Scenario"] <- "scenario"
names(stock_productivity$mediterranean$data)[names(stock_productivity$mediterranean$data) == "Indicator"] <- "indicator"

usethis::use_data(stock_productivity, overwrite = TRUE)
