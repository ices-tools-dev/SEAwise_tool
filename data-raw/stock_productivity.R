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





usethis::use_data(stock_productivity, overwrite = TRUE)


purrr::map(.x = stock_productivity, ~c(unique(.x[[1]]$scenario)))
