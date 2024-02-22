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
    
    card(
      card_header("Social and Economic Effects of and on Fishing", class = "bg-primary"),
      card_body(
        layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("socioeconomic_effects")),
          card_image(
            file = NULL, src = "www/socioeco.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/")))),
    
    card(
      card_header("Ecological Effects on Fisheries", class = "bg-secondary"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "1fr 4fr"),
          heights_equal = "row",
          card_image(
            file = NULL, src = "www/eco.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/"),
          uiOutput(ns("eco_effects"))))),
    
    card(
      card_header("Ecological Effects of Fisheries", class = "bg-success"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "4fr 1fr"),
          uiOutput(ns("fishery_effects")),
          card_image(
            file = NULL, src = "www/fishery.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/")))
      ),
    card(
      card_header("Spatial Management impacts on Ecological Systems and Fisheries", class = "bg-info"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "1fr 4fr"),
          heights_equal = "row",
          card_image(
            file = NULL, src = "www/spatial.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/"),
          uiOutput(ns("spatial_management"))))),
    
    card(
      card_header("Evaluation of Fisheries Management Strategies in an Ecosystem Context", class = "bg-warning"),
      card_body(
        layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("mse")),
          card_image(
            file = NULL, src = "www/mse.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/"))))
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
