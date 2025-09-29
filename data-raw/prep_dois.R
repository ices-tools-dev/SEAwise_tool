## code to prepare `dois` dataset goes here
library(dplyr)

topic <- c("wp2", "wp2_projections", "wp3", "wp4", "wp5", "wp6")
doi <- c("https://doi.org/10.11583/DTU.25634508",
         "https://doi.org/10.11583/DTU.28079393",
         "https://doi.org/10.11583/DTU.28079417",
         "https://doi.org/10.11583/DTU.26075323",
         "https://doi.org/10.11583/DTU.28079366",
         "https://doi.org/10.11583/DTU.29108039")
dois <- tibble(topic, doi)

usethis::use_data(dois, overwrite = TRUE)
