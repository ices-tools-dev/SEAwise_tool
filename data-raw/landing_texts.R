## code to prepare `texts` dataset goes here
library(readxl)

texts_landing <- readxl::read_xlsx("data-raw/landing_texts.xlsx", sheet = "landing_page")
texts2 <- readxl::read_xlsx("data-raw/landing_texts.xlsx", sheet = 2)

texts3 <- readxl::read_xlsx("data-raw/landing_texts.xlsx", sheet = 3)

usethis::use_data(texts_landing, overwrite = TRUE)

