#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Social and Economic", 
               mod_wp2_ui(ns("wp2"))),
      tabPanel("Ecological impacts on Fisheries",
               mod_wp3_ui(ns("wp3"))),
      tabPanel("Ecological consequences of Fisheries",
               mod_wp4_ui(ns("wp4"))),
      tabPanel("WP5"),
      tabPanel("Management Strategy and Trade-off Evaluation"),
    )
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_wp2_server("wp2", case_study)
    mod_wp3_server("wp3", case_study)
    mod_wp4_server("wp4", case_study)
  })
}
    
## To be copied in the UI
# mod_results_ui("results_baltic")
    
## To be copied in the server
# mod_results_server("results_baltic")
