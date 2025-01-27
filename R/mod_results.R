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
      tabPanel("WP2"),
      tabPanel("WP3"),
      tabPanel("WP4"),
      tabPanel("WP5"),
      tabPanel("WP6"),
    )
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_results_ui("results_baltic")
    
## To be copied in the server
# mod_results_server("results_baltic")
