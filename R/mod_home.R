#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body layout_column_wrap
#' @importFrom leaflet leafletOutput leafletProxy hideGroup showGroup 
#' @importFrom shinyWidgets virtualSelectInput updateVirtualSelect
#' @importFrom shinyjs onevent 
#' @importFrom stringr str_replace_all 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(
      card_header("Welcome to the SEAwise Ecosystem Based Fisheries Management Toolbox", class = "bg-primary"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("welcome"))
          ))),
    card(height = "40vh", full_screen = FALSE,
      
      layout_column_wrap(width = 1/2,heights_equal = "all",
        card(min_height = "30vh",
                  tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
                    leafletOutput(ns("map"), width = "100%")
             ),
        
        card(card_body(min_height = "30vh",
              selectInput(
                inputId = ns("selected_locations"),
                label = "",
                choices = c("Please select a case study region", sort(eco_shape$Ecoregion)),
                selected = NULL,selectize = T,
                  
                multiple = FALSE,
                width = "100%")
              ))
        )
    ),
    layout_column_wrap(heights_equal = "row", width = 1/2, fixed_width = FALSE, fillable = T,
      card(
         card_header("Featured SEAwise Research", class = "bg-warning"),
         uiOutput(ns("featured_research"))),
      card(
        card_header("Partners", class = "bg-success"),uiOutput(ns("who")),
                  card_image(file = NULL, src = "img/normal-reproduction-high-resolution.jpg", height = "50px", width = "75px", border_radius = "all"))
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, parent_session, selected_locations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$map <- leaflet::renderLeaflet({
      print("Rendering map")
      print(paste("eco_shape dimensions:", nrow(eco_shape), "x", ncol(eco_shape)))
      print(paste("map_shape dimensions:", nrow(map_shape), "x", ncol(map_shape)))
      map_ecoregion(eco_shape, map_shape)
    })
    
    observeEvent(input$selected_locations, {

      temp_location <- input$selected_locations
      temp_location <- str_replace_all(temp_location, " ", "_")
      temp_location <- tolower(temp_location)
      selected_locations(temp_location)
    
      tab_to_show <- switch(
        selected_locations(),
        "greater_north_sea" = "results_gns",
        "baltic_sea" = "results_baltic",
        "western_waters" = "results_ww",
        "mediterranean" = "results_med",
        "Home"
      )
      updateNavbarPage(session = parent_session, inputId = "main-navbar", selected = tab_to_show)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    output$welcome <- renderUI({
  
      text <- paste(select_text(texts, "landing_page", "welcome"))
      
      HTML(text)
      
    })
    
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

  })
}
    
## To be copied in the UI
# mod_home_ui("home_1")
    
## To be copied in the server
# mod_home_server("home_1")
