## code to prepare `landing_texts` dataset goes here. 
# These are the texts that the user sees within "Results" and are specific to the various ecoregions

library(readxl)
library(dplyr)
library(purrr)


source("data-raw/prep_dois.R")
cat("Dois prepared\n")

# path <- "data-raw/figure_texts_generic.xlsx"
# sheets <- excel_sheets(path)
# texts <- lapply(sheets, read_xlsx, path = path) 
# names(texts) <- sheets
# texts_gns <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

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

path <- "data-raw/figure_texts_cs.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_cs <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

path <- "data-raw/figure_texts_central_med.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_cmed <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

path <- "data-raw/figure_texts_eastern_med.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_emed <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

path <- "data-raw/figure_texts_baltic.xlsx"
sheets <- excel_sheets(path)
texts <- lapply(sheets, read_xlsx, path = path) 
names(texts) <- sheets
texts_baltic <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))





figure_texts <- list("bay_of_biscay" = texts_bob,
                     "baltic_sea" = texts_baltic,
                     "celtic_seas" = texts_cs,
                     "greater_north_sea" = texts_gns,
                     "central_mediterranean" = texts_cmed,
                     "eastern_mediterranean" = texts_emed)


usethis::use_data(figure_texts, overwrite = TRUE)
rm(list = ls())
