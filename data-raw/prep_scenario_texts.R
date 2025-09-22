## code to prepare `scenario_texts` dataset goes here. 
# These are the texts that the user sees within "Results" and are specific to the various ecoregions

library(readxl)
library(dplyr)
library(purrr)

path <- "data-raw/scenario_texts.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) #col_types = "text", col_names = FALSE) 
names(texts) <- sheets
scenario_texts <- map(texts, ~ mutate(., mgmt_generic = paste0("<p>", mgmt_generic, "</p>")))
scenario_texts <- map(texts, ~ mutate(., mgmt_specific = paste0("<p>", mgmt_specific, "</p>")))
scenario_texts <- map(texts, ~ mutate(., climate = paste0("<p>", climate, "</p>")))


usethis::use_data(scenario_texts, overwrite = TRUE)
rm(list = ls())
