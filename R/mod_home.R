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
      card_header("Welcome to the SEAwise Ecosystem Based Fisheries Management Tool", class = "bg-primary"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("welcome"))
          ))),
    card(
      full_screen = FALSE,
      card_header("Select a case study region"),
      tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
        leafletOutput(ns("map"), width = "90%"),
      tags$style(type = "text/css", "#selected_locations {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
      card_body(
        min_height = 400,
        virtualSelectInput(
          inputId = ns("selected_locations"),
          label = "Selected Case study region:",
          choices = sort(eco_shape$Ecoregion),
          selected = NULL,
          multiple = FALSE,
          width = "100%",
          search = TRUE,
          optionsCount = 11
        )
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
    proxy_map <- leafletProxy("map")
    
    # Create empty character vector to hold map selected locations
    selected_map <- reactiveValues(groups = character())
    
    observeEvent(input$map_shape_click, {
      req(!is.null(input$map_shape_click$id))
      
      if (input$map_shape_click$group == "Eco_regions") {
        selected_map$groups <- c(selected_map$groups, input$map_shape_click$id)
      }
      
      updateVirtualSelect(
        inputId = "selected_locations",
        choices = eco_shape$Ecoregion,
        selected = input$map_shape_click$id
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$selected_locations, {
      req(input$selected_locations)
      print(paste("Selected location:", input$selected_locations))
      
      temp_location <- input$selected_locations
      temp_location <- str_replace_all(temp_location, " ", "_")
      temp_location <- tolower(temp_location)
      
      print(paste("Updating selected_ecoregion to:", temp_location))

      
      removed <- setdiff(selected_map$groups, input$selected_locations)
      selected_map$groups <- input$selected_locations
      
      proxy_map %>%
        hideGroup(removed) %>%
        showGroup(input$selected_locations)
      
      # **Update the reactiveVal**
      selected_locations(temp_location)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$selected_locations, {
     
      tab_to_show <- switch(
        input$selected_locations,
        "greater_north_sea" = "Greater North Sea",
        "baltic_sea" = "Baltic Sea",
        "western_waters" = "Western Waters",
        "mediterranean" = "Mediterranean",
        "Home"
      )
      updateNavbarPage(session = parent_session, inputId = "main-navbar", selected = input$selected_locations)
    }, ignoreInit = TRUE)
    
    
    output$welcome <- renderUI({
  
      text <- paste(select_text(texts, "landing_page", "welcome"), 
                    select_text(texts, "landing_page", "website"))
      
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
