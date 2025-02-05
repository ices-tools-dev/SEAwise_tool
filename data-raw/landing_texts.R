## code to prepare `texts` dataset goes here
library(readxl)
library(dplyr)
library(purrr)

path <- "data-raw/landing_texts.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

usethis::use_data(texts, overwrite = TRUE)

