#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  selected_locations <- reactiveVal(NULL)
  
  mod_home_server("home_1", parent_session = session, selected_locations = selected_locations)
  mod_seawise_server("seawise_1")
  mod_themes_server("themes_1")
  mod_case_studies_server("case_studies_1")
  
  # Monitor navbar selection to update selected_locations
  observeEvent(input$`main-navbar`, {
    req(input$`main-navbar`)
    
    # Map navbar values to ecoregion identifiers
    ecoregion_mapping <- list(
      "results_baltic" = "baltic_sea",
      "results_bob" = "bay_of_biscay",
      "results_cs"  = "celtic_seas",
      "results_ns"  = "greater_north_sea",
      "results_cmed" = "central_mediterranean",
      "results_emed" = "eastern_mediterranean",
      "results_ww" = "western_waters"
    )
    
    selected_ecoregion <- ecoregion_mapping[[input$`main-navbar`]]
    
    if (!is.null(selected_ecoregion)) {
      selected_locations(selected_ecoregion)
    }
  })
  
  # Initialize results modules based on selected_locations()
  observeEvent(selected_locations(), {
    req(selected_locations())
    
    # Dynamically call the appropriate results module
    switch(selected_locations(),
           "baltic_sea" = mod_results_server("results_baltic", case_study = selected_locations),
           "bay_of_biscay" = mod_results_server("results_bob", case_study = selected_locations),
           "celtic_seas" = mod_results_server("results_cs", case_study = selected_locations),
           "greater_north_sea" = mod_results_server("results_ns", case_study = selected_locations),
           "central_mediterranean" = mod_results_server("results_cmed", case_study = selected_locations),
           "eastern_mediterranean" = mod_results_server("results_emed", case_study = selected_locations),
           "western_waters" = mod_results_server("results_ww", case_study = selected_locations)
    )
  })
  
  
  selected_locations("bay_of_biscay") # Default
}
