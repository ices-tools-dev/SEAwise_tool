#' seawise UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_seawise_ui <- function(id){
  ns <- NS(id)
  tagList(
   
    layout_column_wrap(heights_equal = "row", width = 1/2, fixed_width = FALSE, fillable = T,
      card(
        card_header("The SEAwise Objective", class = "bg-secondary"),
                  uiOutput(ns("objective"))),
      card(
        card_header("Research Themes", class = "bg-success"),
        uiOutput(ns("themes")))
      ),
    layout_column_wrap(heights_equal = "row", width = 1/3, fixed_width = FALSE, fillable = T,
      card(
        card_header("Case Studies", class = "bg-info"),
        uiOutput(ns("case_study_regions"))),
      card(
        card_header("Partners", class = "bg-warning"),uiOutput(ns("who")),
                  card_image(file = NULL, src = "img/normal-reproduction-high-resolution.jpg", height = "50px", width = "75px", border_radius = "all")),
      card(
        card_header("SEAwise Website", class = "bg-danger"),uiOutput(ns("website"))),
      ),
    layout_column_wrap(heights_equal = "row", width = 1, fixed_width = FALSE, fillable = T,
                       card(
                         card_header("Today's Featured Research", class = "bg-primary"),
                         uiOutput(ns("featured_research")))
    )
  )
}
    
#' seawise Server Functions
#'
#' @noRd 
mod_seawise_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$objective <- renderUI({
      HTML(select_text(texts, "landing_page", "objective"))
    })
    
    output$case_study_regions <- renderUI({
      HTML(select_text(texts, "landing_page", "case_study_regions"))
    })
    
    output$themes <- renderUI({
      HTML(select_text(texts, "landing_page", "themes"))
    })
    
    output$who <- renderUI({
      HTML(select_text(texts, "landing_page", "who"))
    })
    
    output$website <- renderUI({
      HTML(select_text(texts, "landing_page", "website"))
    })
    
  })
}
    
## To be copied in the UI
# mod_seawise_ui("seawise_1")
    
## To be copied in the server
# mod_seawise_server("seawise_1")
