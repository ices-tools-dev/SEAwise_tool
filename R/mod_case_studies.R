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
        bslib::card_body(uiOutput(ns("med_case"))),
        bslib::card_image(
          file = NULL, src = "www/med.png",
                          height = "200px")),
      bslib::card(
        bslib::card_header("Baltic", class = "bg-secondary"),
        bslib::card_body(uiOutput(ns("baltic_case"))),
        bslib::card_image(file = NULL, src = "www/bs.png",
                          height = "200px")),
      bslib::card(
        bslib::card_header("Greater North Sea", class = "bg-success"),
        bslib::card_body(uiOutput(ns("gns_case"))),
        tags$img(src = "www/owl2.png"),
        # bslib::card_image(file = "www/owl2", mime_type = "png", height = "200px")
        )
      ,
      bslib::card(
        bslib::card_header("Western Waters", class = "bg-info"),
        bslib::card_body(uiOutput(ns("ww_case"))),
        bslib::card_image(file = NULL, src = "www/ww.png",
                          height = "200px"))
  )
)
}
    
#' case_studies Server Functions
#'
#' @noRd 
mod_case_studies_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$med_case <- renderUI({
      HTML(select_text(texts, "case_studies", "med_case"))
    })
    
    output$baltic_case <- renderUI({
      HTML(select_text(texts, "case_studies", "baltic_case"))
    })
    
    output$gns_case <- renderUI({
      HTML(select_text(texts, "case_studies", "gns_case"))
    })
    
    output$ww_case <- renderUI({
      HTML(select_text(texts, "case_studies", "ww_case"))
    })
    
  })
}
    
## To be copied in the UI
# mod_case_studies_ui("case_studies_1")
    
## To be copied in the server
# mod_case_studies_server("case_studies_1")
