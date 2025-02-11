#' wp6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp6_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      tabPanel("Management Strategy Evaluation",
               mod_mse_ui(ns("mse_1"))
      ),
      tabPanel("Trade-off analysis",
               mod_mcda_ui(ns("mcda_1")))
    )
  )
}

#' wp6 Server Functions
#'
#' @noRd 
mod_wp6_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_mse_server("mse_1", case_study)
    mod_mcda_server("mcda_1", case_study)
  })
}

## To be copied in the UI
# mod_wp6_ui("wp6_1")

## To be copied in the server
# mod_wp6_server("wp6_1")
