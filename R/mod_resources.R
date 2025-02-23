#' resources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card 
#' 
mod_resources_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(HTML('<p>For questions and feedback relating to the SEAwise EBFM toolbox: <a href="mailto:neil.maginnis@ices.dk">neil.maginnis@ices.dk</a></p>'))
  )
}
    
#' resources Server Functions
#'
#' @noRd 
mod_resources_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_resources_ui("resources_1")
    
## To be copied in the server
# mod_resources_server("resources_1")
