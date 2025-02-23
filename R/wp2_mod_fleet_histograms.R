#' fleet_histograms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2 
mod_fleet_histograms_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(height = "70vh", full_screen = TRUE, max_height = "100%",
         layout_sidebar(sidebar = sidebar(uiOutput(ns("plot_filters"))),
                        plotOutput(ns("fleet_histograms")))
    )
  )
}
    
#' fleet_histograms Server Functions
#'
#' @noRd 
mod_fleet_histograms_server <- function(id, fleet_data, ecoregion){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    data <- reactive({
      dat <- fleet_data()
      colnames(dat) <- tolower(colnames(dat))
      dat
    })
    
    output$plot_filters <- renderUI({
      req(data(), ecoregion())
      countries <- unique(data()$country)
      variables <- unique(data()$variable)
      tagList(
        selectizeInput(ns("country_filter"), "Select Countries", choices = countries, selected = countries, multiple = T),
        selectizeInput(ns("variable_filter"), "Select Fleet variables", choices = variables, selected = variables, multiple = T)
      )
    })
    
    filtered_data <- reactive({
      req(data(), input$country_filter, input$variable_filter)
      data() %>% filter(country %in% input$country_filter, variable %in% input$variable_filter)
    })
    
    output$fleet_histograms <- renderPlot({
      req(nrow(filtered_data()) > 0, ecoregion(), input$country_filter, input$variable_filter)
      
      ggplot(data=filtered_data(), aes(x=year, y=value, colour=fleet)) + 
        geom_point(inherit.aes = T, , size = 1.5)+
        geom_line(inherit.aes = T, stat="identity",size=1)+
        # facet_wrap(country + variable~.,scales="free_y",drop=FALSE,ncol=6)+
        facet_grid(variable ~ country, scales="free_y", drop=FALSE, )+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.spacing = unit(1, "lines"))
      
    })
    
  })
}
    
## To be copied in the UI
# mod_fleet_histograms_ui("fleet_histograms_1")
    
## To be copied in the server
# mod_fleet_histograms_server("fleet_histograms_1")
