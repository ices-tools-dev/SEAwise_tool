## code to prepare `data/` datasets goes here
library(dplyr)
load("data-raw/wp6/D6_11_catch.RData")
load("data-raw/wp6/D6_11_stock.RData")


total_landings_stock <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, stock) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum)  %>% group_by(region, case_study, fleet_dynamics, HCR, stock) %>% 
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc    = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, stock, period) %>% 
  mutate(position = length(stock), stock = reorder(as.factor(stock), position))


total_landings_fleet <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, fleet) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum) %>%  
  group_by(region, case_study, fleet_dynamics, HCR, fleet) %>%  
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc     = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, fleet, period) %>% 
  mutate(position = length(fleet), fleet = reorder(as.factor(fleet), position))


tab_stock <- stock %>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_f_fmsy', 'ratio_average_age', 'ratio_ssb'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR)


tab_stock_land <- total_landings_stock %>% filter(!(region == 'Bay of Biscay' & ratio_landings == Inf))%>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_landings'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR) 


tab_stock <- tab_stock %>% bind_cols(tab_stock_land[,-(1:4)]) %>% tidyr::pivot_longer(cols = 5:8, names_to = 'indicator') %>% 
  mutate(value_trunc = ifelse(value > 100,100, value))



mse_plot_params <- list(
        SSB = list(y = "stock",
                        var = "ratio_ssb_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "Change\n in SSB",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock",
                        hline = c(5, 21,29,34,36)),
        F_ratio = list(y = "stock",
                        var = "ratio_f_fmsy_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "F/Fmsy",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock",
                        hline = c(5, 21,29,34,36)),
        mean_age = list(y = "stock",
                        var = "ratio_average_age_trunc",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 0,
                        title = "Change in\n average age",
                        lims = c(-100,100),
                        breaks = seq(-100, 100, 50),
                        y_lab = "Stock",
                        hline = c(5, 21,29,34,36)),
        ssb_blim = list(y = "stock",
                        var = "pBlim",
                        col = list(low = "darkblue",
                                   mid = "white",
                                   high = "darkred"),
                        midpoint = 5,
                        title = "p(SSB < Blim)",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock",
                        hline = c(5, 21,29,34,36)),
  fleet_landings = list(y = "fleet",
                        var = "ratio_landings_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Fleet",
                        hline = c(3, 19,35,45,47)),
  fleet_landings_value = list(y = "fleet",
                        var = "ratio_value_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Fleet",
                        hline = c(3,19,35,45,47)),
  stock_landings = list(y = "stock",
                        var = "ratio_landings_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock",
                        hline = c(5, 29,37,42,47)),
  stock_landings_value = list(y = "stock",
                        var = "ratio_value_trunc",
                        col = list(low = "white",
                                   mid = "darkblue",
                                   high = "darkgreen"),
                        midpoint = 50,
                        title = "Change\n in Landings",
                        lims = c(0,100),
                        breaks = seq(0, 100, 25),
                        y_lab = "Stock",
                        hline = c(5, 29,37,42,47))
)


constant_cpue <- c("NEP", "POL", "RJC", "RJN", "RJU", "SDV", "MUR", "NEP10",       
                   "NEP32", "NEP33",  "NEP34",  "NEP5", "NEP6", "NEP7", 
                   "NEP8", "NEP9",  "NEPOTH-NS", "WHG", "OTR", "DPS")

usethis::use_data(stock, overwrite = T)
usethis::use_data(catch, overwrite = T)
usethis::use_data(tab_stock, overwrite = T)
usethis::use_data(tab_stock_land, overwrite = T)
usethis::use_data(total_landings_fleet, overwrite = T)
usethis::use_data(total_landings_stock, overwrite = T)
usethis::use_data(mse_plot_params, overwrite = T)
usethis::use_data(constant_cpue, overwrite = T)
