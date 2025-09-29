# Script to wrangle fish portion data.

library(readxl)
library(dplyr)  
sheets <- excel_sheets("data-raw/wp2/Fish_portions.xlsx")

fish_portions <- lapply(sheets, read_xlsx, path = "data-raw/wp2/Fish_portions.xlsx")

names(fish_portions) <- sheets
names(fish_portions)[1] <- "NS"

ns <- fish_portions[[1]]
ns <- mutate(ns, adult_portions = as.numeric(adult_portions))
ns$spec = gsub("[0-9]+|-NS|-EC|OTH","",ns$Stock)
fish_portions[[1]] <- ns %>% dplyr::group_by(Country, Fleet, Stock = spec) %>% 
  dplyr::summarise(adult_portions = sum(adult_portions))


usethis::use_data(fish_portions, overwrite = TRUE)
