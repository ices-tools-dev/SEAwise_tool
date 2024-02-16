#' themes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_themes_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Social and Economic Effects of and on Fishing", class = "bg-primary"),
      bslib::card_body(uiOutput(ns("socioeconomic_effects"))),
      bslib::card_image(file = NULL, src = "www/socioeco.png",
                        height = "200px",
                        href = "https://ices-taf.shinyapps.io/seawise/")
      ),
    bslib::card(
      bslib::card_header("Ecological Effects on Fisheries", class = "bg-secondary"),
      bslib::card_body(uiOutput(ns("eco_effects"))),
      bslib::card_image(file = NULL, src = "socioeco",
                        height = "200px",
                        href = "https://ices-taf.shinyapps.io/seawise/")),
    bslib::card(
      bslib::card_header("Ecological Effects of Fisheries", class = "bg-success"),
      bslib::card_body(uiOutput(ns("fishery_effects")))),
    bslib::card(
      bslib::card_header("Spatial Management impacts on Ecological Systems and Fisheries", class = "bg-info"),
      bslib::card_body(uiOutput(ns("spatial_management")))),
    bslib::card(
      bslib::card_header("Evaluation of Fisheries Management Strategies in an Ecosystem Context", class = "bg-warning"),
      bslib::card_body(uiOutput(ns("mse"))))
  )
}
    
#' themes Server Functions
#'
#' @noRd 
mod_themes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$socioeconomic_effects <- renderUI({
      HTML(select_text(texts, "themes", "socioeconomic_effects"))
    })
    
    output$eco_effects <- renderUI({
      HTML(select_text(texts, "themes", "eco_effects"))
    })
    
    output$fishery_effects <- renderUI({
      HTML(select_text(texts, "themes", "fishery_effects"))
    })
    
    output$spatial_management <- renderUI({
      HTML(select_text(texts, "themes", "spatial_management"))
    })
    
    output$mse <- renderUI({
      HTML(select_text(texts, "themes", "mse"))
    })
    
  })
}
    
## To be copied in the UI
# mod_themes_ui("themes_1")
    
## To be copied in the server
# mod_themes_server("themes_1")
