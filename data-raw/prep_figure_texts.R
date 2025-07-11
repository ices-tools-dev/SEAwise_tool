library(readxl)
library(dplyr)
library(purrr)

path <- "data-raw/figure_texts_bob.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_bob <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

path <- "data-raw/figure_texts_ns.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_gns <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

figure_texts <- list("bay_of_biscay" = texts_bob,
                     "greater_north_sea" = texts_gns)


usethis::use_data(figure_texts, overwrite = TRUE)
rm(list = ls())
