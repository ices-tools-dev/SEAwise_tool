## code to prepare `colours` dataset goes here

seawise_theme <- bslib::bs_theme(version = 5,
                                 bg = "#FFFFFF",
                                 fg = "#000000",
  primary = "#210384",
  secondary = "#037184",
  success = "#00B292",
  info = "#00B262",
  warning = "#86C64E",
  danger = "#C6E83E"
)

bslib::bs_theme_preview(theme = seawise_theme
                        )
# seawise_theme <- bslib::bs_theme_update(seawise_theme, preset = "flatly")
seawise_colours <- c(
  "#210384",
  "#037184",
  "#00B292",
  "#00B262",
  "#86C64E",
  "#C6E83E"
)

usethis::use_data(seawise_theme, seawise_colours, overwrite = TRUE, internal = T)
