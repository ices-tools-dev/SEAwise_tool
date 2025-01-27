#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  mod_home_server("home_1", parent_session = session)
  mod_seawise_server("seawise_1")
  mod_themes_server("themes_1")
  mod_case_studies_server("case_studies_1")
  mod_results_server("results_baltic")
  mod_results_server("results_ns")
  mod_results_server("results_ww")
  mod_results_server("results_med")
}
