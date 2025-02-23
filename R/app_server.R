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
  mod_resources_server("resources_1")
  
  # Monitor navbar selection to update selected_locations
  observeEvent(input$`main-navbar`, {
    # Check if the selected tab is one of the "Results" tabs
    if (input$`main-navbar` %in% c("results_baltic", "results_gns", "results_med", "results_ww")) {
      # Update selected_locations based on the results tab selected
      # Dynamically call the appropriate results module
      results_tab <- switch(input$`main-navbar`,
                            "results_baltic" = "baltic_sea",
                            "results_gns" = "greater_north_sea",
                            "results_med" = "mediterranean",
                            "results_ww" = "western_waters"
      )
      selected_locations(results_tab)
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
           "mediterranean" = mod_results_server("results_med", case_study = selected_locations),
           "western_waters" = mod_results_server("results_ww", case_study = selected_locations)
    )
  })
  
  
}
