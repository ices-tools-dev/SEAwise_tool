library(readxl)
library(dplyr)
library(purrr)

path <- "data-raw/context_texts.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
context_texts <- map_df(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

usethis::use_data(context_texts, overwrite = TRUE)

rm(list = ls())
