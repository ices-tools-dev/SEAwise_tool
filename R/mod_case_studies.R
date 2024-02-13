#' case_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_case_studies_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      bslib::card(
        bslib::card_header("Mediterreanean", class = "bg-warning"),
        bslib::card_body(uiOutput(ns("med_case")))),
      bslib::card(
        bslib::card_header("Baltic", class = "bg-secondary"),
        bslib::card_body(uiOutput(ns("baltic_case")))),
      bslib::card(
        bslib::card_header("Greater North Sea", class = "bg-success"),
        bslib::card_body(uiOutput(ns("gns_case")))),
      bslib::card(
        bslib::card_header("Western Waters", class = "bg-info"),
        bslib::card_body(uiOutput(ns("ww_case"))))
  )
)
}
    
#' case_studies Server Functions
#'
#' @noRd 
mod_case_studies_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_case_studies_ui("case_studies_1")
    
## To be copied in the server
# mod_case_studies_server("case_studies_1")
